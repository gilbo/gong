
#ifdef USE_SCAN
#include "sphere_uniform_scan.h"
#endif
#ifdef USE_BVH
#include "sphere_uniform_bvh.h"
#endif
#ifdef USE_HASH
#include "sphere_uniform_hash.h"
#endif
#ifdef USE_GRID
#include "sphere_uniform_grid.h"
#endif

#include <iostream>
#include <iomanip>
#include <cmath>
//#include <set>
//#include <utility>
#include <vector>
#include <string>
#include <assert.h>
#include <fstream>
using namespace std;

#include "vdb.h"

#include "sys/time.h"
#include "unistd.h"

double taketime() {
  timeval tv;
  gettimeofday(&tv,0);
  return double(tv.tv_sec) + 1e-6*double(tv.tv_usec);
}

#define CHECK_ERR() { \
  if(store.geterror()) { \
    cerr << store.geterror() << endl; \
    exit(1); \
} }

#define ERR(msg) { \
 \
  cerr << "ERROR: " << msg << endl; \
  exit(1); \
}

typedef tensor_float_3_           vec3;
typedef float                     num;


void LoadStore( Store store, string filename )
{
  vec3 v;
  v.d[0] = 0;
  v.d[1] = 0;
  v.d[2] = 0;

  ifstream YF(filename);
  if(!YF.good()) { ERR("could not open '" << filename << "'"); }

  vector<vec3> pos;
  int n_yarns, n_samp;

  YF >> n_yarns;
  for(int i=0; i<n_yarns; i++) {
    YF >> n_samp;
    for(int k=0; k<n_samp; k++) {
      YF >> v.d[0] >> v.d[1] >> v.d[2];
      pos.push_back(v);
    }
  }

  if(!YF.good()) { ERR("file-read error in '" << filename << "'"); }
  YF.close();

  store.Spheres().beginload(pos.size());
  for(int k=0; k<pos.size(); k++)
    store.Spheres().loadrow( pos[k], 0 );
  store.Spheres().endload();
}

struct PerfFile {
  vector<int> frames;
  vector<int> ms;
};

void ReadPerf( PerfFile &file, string filename ) {
  ifstream F(filename);
  if(!F.good()) { ERR("could not open '" << filename << "'"); }

  int frame, ms;
  string tmpstr;
  while(getline(F, tmpstr, ',')) {
    frame = stoi(tmpstr);
    getline(F, tmpstr, '\n');
    ms = stoi(tmpstr);
    //cout << "READ " << frame << ' ' << ms << endl;
    file.frames.push_back(frame);
    file.ms.push_back(ms);
  }

  //if(!F.good()) { ERR("file-read error in '" << filename << "'"); }
  F.close();
}



void SphereCounts( Store store )
{
  int total_collisions  = 0;

  int n_spheres = store.Spheres().size();

  uint32_t *ic          = store.Spheres().isct_count().read_lock();
  for(uint32_t k=0; k<n_spheres; k++) {
    total_collisions += ic[k];
  }
  store.Spheres().isct_count().read_unlock();
  //cout << "  Found " << total_collisions/2 << " collisions" << endl;
}

/*
const int SPHERE_DIV = 8;
vec3 SPHERE_PTS[SPHERE_DIV][SPHERE_DIV];
void SPHERE_PRECOMP() {
  for(int x=0; x<SPHERE_DIV; x++) {
    for(int y=0; y<SPHERE_DIV; y++) {
      num rad_x = 2*M_PI * (num(x)/num(SPHERE_DIV));
      num rad_y = M_PI * (num(y)/num(SPHERE_DIV));
      num sinx  = sin(rad_x);
      num cosx  = cos(rad_x);
      num siny  = sin(rad_y);
      num cosy  = cos(rad_y);
      num X     = siny * cosx;
      num Y     = siny * sinx;
      num Z     = cosy;
      SPHERE_PTS[x][y].d[0] = X;
      SPHERE_PTS[x][y].d[1] = Y;
      SPHERE_PTS[x][y].d[2] = Z;
    }
  }
}*/
vec3 vmadf( vec3 a, num s, vec3 b ) {
  vec3 r;
  r.d[0] = a.d[0] + s * b.d[0];
  r.d[1] = a.d[1] + s * b.d[1];
  r.d[2] = a.d[2] + s * b.d[2];
  return r;
}
vec3 cross( vec3 a, vec3 b ) {
  vec3 r;
  r.d[0] =  a.d[1] * b.d[2]  -  a.d[2] * b.d[1];
  r.d[1] = -a.d[0] * b.d[2]  +  a.d[2] * b.d[0];
  r.d[2] =  a.d[0] * b.d[1]  -  a.d[1] * b.d[0];
  return r;
}
vec3 sub( vec3 a, vec3 b ) {
  vec3 r;
  r.d[0] = a.d[0] - b.d[0];
  r.d[1] = a.d[1] - b.d[1];
  r.d[2] = a.d[2] - b.d[2];
  return r;
}
void norm_color(vec3 n) {
  num n2 = n.d[0]*n.d[0] + n.d[1]*n.d[1] + n.d[2]*n.d[2];
  if (n2 < 1e-7) { vdb_color(0,0,0); }
  else {
    num nlen = sqrt(n2) * sqrt(3);
    num f = (n.d[0] + n.d[1] + n.d[2]) / nlen;
    num c = 0.5*(1+f);
    vdb_color(c,c,c);
  }
}
void v_quad( vec3 lolo, vec3 hilo, vec3 hihi, vec3 lohi )
{
  vec3 c0 = cross(sub(hilo,lolo),sub(hihi,lolo));
  vec3 c1 = cross(sub(hihi,lolo),sub(lohi,lolo));
  vec3 n  = vmadf(c0,1,c1);
  norm_color( n );
  vdb_triangle( lolo.d[0], lolo.d[1], lolo.d[2],
                hilo.d[0], hilo.d[1], hilo.d[2],
                hihi.d[0], hihi.d[1], hihi.d[2]);
  vdb_triangle( lolo.d[0], lolo.d[1], lolo.d[2],
                hihi.d[0], hihi.d[1], hihi.d[2],
                lohi.d[0], lohi.d[1], lohi.d[2] );
}
/*
void vdb_sphere( vec3 pos, num r )
{
  vec3 pts[SPHERE_DIV][SPHERE_DIV];
  for(int x=0; x<SPHERE_DIV; x++)
    for(int y=0; y<SPHERE_DIV; y++)
      pts[x][y] = vmadf(pos, r, SPHERE_PTS[x][y]);
  for(int x=0; x<SPHERE_DIV; x++) {
    for(int y=0; y<SPHERE_DIV; y++) {
      int nx = (x==SPHERE_DIV-1)? 0 : x+1;
      int ny = (y==SPHERE_DIV-1)? 0 : y+1;
      v_quad( pts[x][y], pts[nx][y], pts[nx][ny], pts[x][ny] );
    }
  }
}*/

void DrawSpheres( Store store )
{
  vdb_begin();
  vdb_frame();

  num RADIUS = 1.0;

  int n_spheres = store.Spheres().size();

  vec3 *pos         = store.Spheres().pos().read_lock();
  for(int k=0; k<n_spheres; k++) {
    //vdb_sphere(pos, RADIUS);
    vdb_point(pos[k].d[0], pos[k].d[1], pos[k].d[2]);
  }
  store.Spheres().pos().read_unlock();

  vdb_end();
}


// --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
//   --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --    

int main() {
  Store store  = Store::NewStore();
  CHECK_ERR();

  //InitStore(store, N_SPHERES);
  //SPHERE_PRECOMP();

  double min_time   = 1e6;
  double max_time   = 0;
  double sum_time   = 0;

  string DIR = "../../../yarnsim/results/alt-diag/";

  PerfFile pfile;
  ReadPerf(pfile, DIR + "contactperf.txt");

  for(int k=0; k < pfile.frames.size(); k++) {
    int frame = pfile.frames[k];
    LoadStore(store, DIR + "frame" + to_string(frame) + ".txt");

    double start = taketime();
#ifdef GPU_ENABLE
    store.sphere_self_isct_GPU();
#else
    store.sphere_self_isct();
#endif
    // force synced timing if on GPU
    store.Spheres().pos().read_lock();
    store.Spheres().pos().read_unlock();
    double stop = taketime();

    DrawSpheres(store);    

    CHECK_ERR();
    SphereCounts(store);
    double dtime = stop - start;
    sum_time += dtime;
    if (dtime < min_time) min_time = dtime;
    if (dtime > max_time) max_time = dtime;
    //cout << "frame #" << frame << " collision took "
    //     << (stop-start)*1e3 << " ms" << endl;
  }

  double min_ys_time = 1e6;
  double max_ys_time = 0;
  double sum_ys_time = 0;
  for(int k=0; k < pfile.ms.size(); k++) {
    double ms = 1e-3 * (pfile.ms[k]);
    sum_ys_time += ms;
    if (ms < min_ys_time) min_ys_time = ms;
    if (ms > max_ys_time) max_ys_time = ms;
  }

  int N_FRAMES = pfile.frames.size();

  cout << "Ran collisions for " << N_FRAMES << " frames" << endl;
  cout << "                on " << store.Spheres().size()
                                << " spheres" << endl;

  double avg_ys_time = sum_ys_time / N_FRAMES;
  cout << "YARNSIM frame timings (in ms) for " << pfile.frames.size()
                                       << " frames" << endl;
  cout << "    avg         min         max         " << endl;
  cout << "    "
       << setw(10) << avg_ys_time << "  "
       << setw(10) << min_ys_time << "  "
       << setw(10) << max_ys_time << "  "
       << endl;

  double avg_time = sum_time / N_FRAMES;
  cout << "frame timings (in ms) for " << pfile.frames.size()
                                       << " frames" << endl;
  cout << "    avg         min         max         " << endl;
  cout << "    "
       << setw(10) << avg_time*1e3 << "  "
       << setw(10) << min_time*1e3 << "  "
       << setw(10) << max_time*1e3 << "  "
       << endl;

  store.print_profile();

  cout << "sphere_uniform done" << endl;
  return 0;
}



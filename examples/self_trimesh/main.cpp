
#ifdef USE_SCAN
#include "self_trimesh_scan.h"
#endif
#ifdef USE_BVH
#include "self_trimesh_bvh.h"
#endif


#include <iostream>
#include <cmath>
#include <set>
#include <utility>
#include <vector>
#include <string>
#include <assert.h>
#include <fstream>
using namespace std;


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
typedef tensor_row_Verts__3_      tri3;

void MeshLoad( Store store, uint32_t n_vert, uint32_t n_tri,
               float* pos, uint32_t* vert )
{
  // build a set of edges
  set< pair<uint32_t,uint32_t> >  edges;
  for(uint32_t i=0; i<n_tri; i++) {
    uint32_t v0;
    uint32_t v1;
    v0            = vert[3*i+0];
    v1            = vert[3*i+1];
    if(v1 < v0) { swap(v0,v1); }
    edges.insert(make_pair(v0, v1));
    v0            = vert[3*i+0];
    v1            = vert[3*i+2];
    if(v1 < v0) { swap(v0,v1); }
    edges.insert(make_pair(v0, v1));
    v0            = vert[3*i+1];
    v1            = vert[3*i+2];
    if(v1 < v0) { swap(v0,v1); }
    edges.insert(make_pair(v0, v1));
  }
  uint32_t n_edge   = edges.size();

  // convert the set of edges into arrays
  vector<uint32_t> e_hd(n_edge);
  vector<uint32_t> e_tl(n_edge);
  uint32_t write = 0;
  for(auto ep : edges) {
    e_tl[write] = ep.first;
    e_hd[write] = ep.second;
    write++;
  }
  assert(write == n_edge);

  store.Verts().beginload(n_vert);
  store.Edges().beginload(n_edge);
  store.Tris().beginload(n_tri);
    store.Verts().pos().load(reinterpret_cast<vec3*>(pos), 3*sizeof(float));
    store.Edges().hd().load(e_hd.data(), sizeof(uint32_t));
    store.Edges().tl().load(e_tl.data(), sizeof(uint32_t));
    store.Tris().v().load(reinterpret_cast<tri3*>(vert), 3*sizeof(uint32_t));
  // Do not endload here
}

void MeshEndLoad( Store store )
{
  store.Verts().endload();
  store.Edges().endload();
  store.Tris().endload();
}

void MeshLoadOFF( Store store, string filename )
{
  // read the file
  ifstream OFF(filename);
  if(!OFF.good()) { ERR("could not open '" << filename << "'"); }
  
  uint32_t n_vert   = 0;
  uint32_t n_tri    = 0;
  uint32_t junk;
  string OFF_sig;
  OFF >> OFF_sig >> n_vert >> n_tri >> junk;
  if(!OFF.good()) { ERR("file-read error in '" << filename << "'"); }
  if(OFF_sig != "OFF") { ERR("expected OFF file marker"); }

  vector<float>     pos(3*n_vert);
  vector<uint32_t>  vert(3*n_tri);

  for(uint32_t k=0; k<n_vert; k++) {
    OFF >> pos[3*k+0] >> pos[3*k+1] >> pos[3*k+2];
  }
  if(!OFF.good()) { ERR("file-read error in '" << filename << "'"); }
  for(uint32_t k=0; k<n_tri; k++) {
    uint32_t count;
    OFF >> count >> vert[3*k+0] >> vert[3*k+1] >> vert[3*k+2];
    if(count != 3) { ERR("expected only triangular faces"); }
  }
  if(!OFF.good()) { ERR("file-read error in '" << filename << "'"); }

  OFF.close();

  MeshLoad( store, n_vert, n_tri, pos.data(), vert.data() );
}

class IsctObj {
public:
  uint32_t    edge_id;
  uint32_t    tri_id;
  vec3        isct_pos;

  uint32_t    edge_v[2];
  vec3        edge_pos[2];
  uint32_t    tri_v[3];
  vec3        tri_pos[3];

  IsctObj(Store store, uint32_t ei, uint32_t ti, vec3 i_pos,
                       uint32_t *e_hd, uint32_t *e_tl, tri3 *tv, vec3 *pos)
  : edge_id(ei), tri_id(ti), isct_pos(i_pos)
  {
    edge_v[0]     = e_tl[ei];
    edge_v[1]     = e_hd[ei];
    tri3 t_v      = tv[ti];

    edge_pos[0]   = pos[edge_v[0]];
    edge_pos[1]   = pos[edge_v[1]];
    for(int k=0; k<3; k++) {
      tri_v[k]    = t_v.d[k];
      tri_pos[k]  = pos[t_v.d[k]];
    }
  }
};
inline ostream&
operator<<(ostream &out, vec3 o) {
  out << "(" << o.d[0] << "," << o.d[1] << "," << o.d[2] << ")";
  return out;
}
inline ostream&
operator<<(ostream &out, IsctObj o) {
  out << "  " << "e(" << o.edge_v[0] << "," << o.edge_v[1] << ")"
      << "  " << "t[" << o.tri_id << "]("
              << o.tri_v[0] << "," << o.tri_v[1] << "," << o.tri_v[2] << ")"
      << "  " << "isct" << o.isct_pos << endl;
  out << "    " << "e[0]=" << o.edge_pos[0] << "  "
                << "e[1]=" << o.edge_pos[1] << endl;
  out << "    " << "t[0]=" << o.tri_pos[0] << "  "
                << "t[1]=" << o.tri_pos[1] << "  "
                << "t[2]=" << o.tri_pos[2] << endl;
  return out;
}

void MeshIsctPrint( Store store )
{
  uint32_t n_isct     = store.ETcontacts().size();
  cout << "Found " << n_isct << " edge-triangle intersections." << endl;

  uint32_t* e           = store.ETcontacts().edge().readwrite_lock();
  uint32_t* t           = store.ETcontacts().tri().readwrite_lock();
  vec3*     isct_pos    = store.ETcontacts().pos().readwrite_lock();
  //...
  uint32_t *e_hd        = store.Edges().hd().readwrite_lock();
  uint32_t *e_tl        = store.Edges().tl().readwrite_lock();
  tri3     *t_v         = store.Tris().v().readwrite_lock();
  vec3     *pos         = store.Verts().pos().readwrite_lock();
  for(uint32_t k=0; k<n_isct; k++) {
    cout << IsctObj(store, e[k], t[k], isct_pos[k], e_hd, e_tl, t_v, pos);
  }
  store.Edges().hd().readwrite_unlock();
  store.Edges().tl().readwrite_unlock();
  store.Tris().v().readwrite_unlock();
  store.Verts().pos().readwrite_unlock();
  //...
  store.ETcontacts().edge().readwrite_unlock();
  store.ETcontacts().tri().readwrite_unlock();
  store.ETcontacts().pos().readwrite_unlock();
}

struct IsctCheckItem {
  uint32_t  t_id;
  uint32_t  e_tl;
  uint32_t  e_hd;
  IsctCheckItem() {}
  IsctCheckItem(uint32_t t, uint32_t e0, uint32_t e1)
    : t_id(t), e_tl(e0), e_hd(e1) {}
};
inline bool
operator==(IsctCheckItem lhs, IsctCheckItem rhs) {
  return lhs.t_id == rhs.t_id &&
         lhs.e_tl == rhs.e_tl &&
         lhs.e_hd == rhs.e_hd;
}
inline bool
operator<(IsctCheckItem lhs, IsctCheckItem rhs) {
  return lhs.t_id < rhs.t_id || ( lhs.t_id == rhs.t_id &&
          ( lhs.e_tl < rhs.e_tl || ( lhs.e_tl == rhs.e_tl && 
            ( lhs.e_hd < rhs.e_hd ) )));
}
inline ostream&
operator<<(ostream & out, IsctCheckItem i) {
  return out << "(t_id=" << i.t_id << ", e_tl=" << i.e_tl
                                   << ", e_hd=" << i.e_hd << ")";
}
void MeshIsctCheck( Store store, string filename )
{
  vector<IsctCheckItem>   isct_ref;
  {
    // read the file
    ifstream F(filename);
    if(!F.good()) { ERR("could not open '" << filename << "'"); }
    
    uint32_t n_contact    = 0;
    string CHECK_sig;
    F >> CHECK_sig >> n_contact;
    if(!F.good()) { ERR("file-read error in '" << filename << "'"); }
    if(CHECK_sig != "CONTACT_RESULTS") {
      ERR("expected CONTACT_RESULTS file marker");
    }

    isct_ref.resize(n_contact);
    for(uint32_t k=0; k<n_contact; k++) {
      uint32_t t,e0,e1;
      F >> t >> e0 >> e1;
      isct_ref[k] = IsctCheckItem(t,e0,e1);
    }
    if(!F.good()) { ERR("file-read error in '" << filename << "'"); }

    F.close();
  }

  // Ok, now we have isct_ref, and we can run the check.  We need
  // to build a set however...
  {
    uint32_t n_isct       = store.ETcontacts().size();
    if(n_isct != isct_ref.size()) {
      ERR("Found " << n_isct << " edge-triangle intersections, "
                   << "but expected " << isct_ref.size());
    }

    set<IsctCheckItem>      found_iscts;
    uint32_t* e           = store.ETcontacts().edge().readwrite_lock();
    uint32_t* t           = store.ETcontacts().tri().readwrite_lock();
    uint32_t* tl          = store.Edges().tl().readwrite_lock();
    uint32_t* hd          = store.Edges().hd().readwrite_lock();
    for(uint32_t k=0; k<n_isct; k++) {
      uint32_t e0         = tl[e[k]];
      uint32_t e1         = hd[e[k]];
      found_iscts.insert( IsctCheckItem(t[k], e0, e1) );
    }
    store.Edges().tl().readwrite_unlock();
    store.Edges().hd().readwrite_unlock();
    store.ETcontacts().edge().readwrite_unlock();
    store.ETcontacts().tri().readwrite_unlock();

    if(found_iscts.size() != n_isct) {
      ERR("Found " << n_isct - found_iscts.size() << " duplicate "
                   << "intersections.");
    }

    // do the check via lookups
    for( auto i : isct_ref ) {
      if(found_iscts.count(i) == 0) {
        ERR("Could not find expected intersection " << i);
      }
    }
  }
}


int main() {
  Store store  = Store::NewStore();
  CHECK_ERR();

  cout << "two_tri CPU" << endl;
  {
    MeshLoadOFF(store, "two_tri.off");
    MeshEndLoad(store);

    store.find_et_iscts();

    MeshIsctPrint(store);
    MeshIsctCheck(store, "two_tri.check");
  }

#ifdef GPU_ENABLE
  cout << "two_tri GPU" << endl;
  {
    MeshLoadOFF(store, "two_tri.off");
    MeshEndLoad(store);

    store.find_et_iscts_GPU();

    MeshIsctPrint(store);
    MeshIsctCheck(store, "two_tri.check");
  }
#endif /* GPU_ENABLE */

  cout << "clothfold CPU" << endl;
  {
    MeshLoadOFF(store, "clothfold.off");
    MeshEndLoad(store);

    store.find_et_iscts();

    MeshIsctCheck(store, "clothfold.check");
  }

#ifdef GPU_ENABLE
  cout << "clothfold GPU" << endl;
  {
    MeshLoadOFF(store, "clothfold.off");
    MeshEndLoad(store);

    store.find_et_iscts_GPU();

    MeshIsctCheck(store, "clothfold.check");
  }
#endif /* GPU_ENABLE */

  cout << "self_trimesh done" << endl;
  return 0;
}


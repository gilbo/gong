
#include "raycaster_gong.h"
#include "off_readwrite.h"

using std::vector;

#define CHECK_ERR() { \
  if(store.geterror()) { \
    std::cerr << store.geterror() << std::endl; \
    exit(1); \
} }

typedef tensor_float_3_           vec3;
typedef tensor_row_Verts__3_      tri3;


void MeshLoad( Store store, uint32_t n_vert, uint32_t n_tri,
               float* pos, uint32_t* vert )
{
  store.Verts().beginload(n_vert);
  store.Tris().beginload(n_tri);
    store.Verts().pos().load(reinterpret_cast<vec3*>(pos), 3*sizeof(float));
    store.Tris().v().load(reinterpret_cast<tri3*>(vert), 3*sizeof(uint32_t));
  store.Verts().endload();
  store.Tris().endload();
}

void RayLoad( Store store, uint32_t n_rays,
               float* origin, float* direction, float* tMax )
{
  store.Rays().beginload(n_rays);
    store.Rays().origin().load(reinterpret_cast<vec3*>(origin), 3*sizeof(float));
    store.Rays().direction().load(reinterpret_cast<vec3*>(direction), 3*sizeof(float));
    store.Rays().tMax().load(tMax, sizeof(float));
  store.Rays().endload();
}


void MeshLoadOFF( Store store, std::string filename )
{
  vector<float>     positions;
  vector<uint32_t>  indices;
  loadOFF(filename, positions, indices);
  uint32_t n_vert = positions.size()/3;
  uint32_t n_tri = indices.size()/3;
  MeshLoad( store, n_vert, n_tri, 
    positions.data(), indices.data() );
}

void RayLoadRFF( Store store, std::string filename )
{
  vector<float>     origins;
  vector<float>     directions;
  loadRFF(filename, origins, directions);
  vector<float>     tMax;
  tMax.resize(origins.size()/3);
  for (int i = 0 ; i < tMax.size(); ++i) {
    printf("Ray ori. %d: %f %f %f\n", i, origins[i*3+0], origins[i*3+1], origins[i*3+2]);
    printf("Ray dir. %d: %f %f %f\n", i, directions[i*3+0], directions[i*3+1], directions[i*3+2]);
    tMax[i] = std::numeric_limits<float>::infinity();
  }

  RayLoad( store, tMax.size(), origins.data(), directions.data(), tMax.data() );
}


void MeshIsctPrintBoolIntersection( Store store )
{
  
  uint32_t n_isct       = store.BoolIntersections().size();
  printf("Num ray tri intersections: %u\n", n_isct);
  uint32_t* r           = store.BoolIntersections().ray().readwrite_lock();
  for(uint32_t k=0; k<n_isct; k++) {
    printf("%u: ray %u\n", k, r[k]);
  }
  store.BoolIntersections().ray().readwrite_unlock();
}

void MeshIsctPrintRayTriIntersection( Store store )
{
  
  uint32_t n_isct       = store.RayTriIntersections().size();
  printf("Num ray tri intersections: %u\n", n_isct);
  uint32_t* t           = store.RayTriIntersections().tri().readwrite_lock();
  uint32_t* r           = store.RayTriIntersections().ray().readwrite_lock();
  for(uint32_t k=0; k<n_isct; k++) {
    printf("%u: ray %u / tri %u\n", k, r[k], t[k]);
  }
  store.RayTriIntersections().ray().readwrite_unlock();
  store.RayTriIntersections().tri().readwrite_unlock();
  
}

void MeshIsctPrintFullIntersection( Store store )
{
  
  uint32_t n_isct       = store.FullIntersections().size();
  printf("Num ray tri intersections: %u\n", n_isct);
  uint32_t* tri          = store.FullIntersections().tri().readwrite_lock();
  uint32_t* r           = store.FullIntersections().ray().readwrite_lock();
  float* t              = store.FullIntersections().t().readwrite_lock();
  float* b              = (float*)store.FullIntersections().barys().readwrite_lock();
  for(uint32_t k=0; k<n_isct; k++) {
    printf("%u: ray %u / tri %u: %f (%f, %f, %f)\n", k, r[k], tri[k], t[k], b[k*3+0],b[k*3+1],b[k*3+2]);
  }
  store.FullIntersections().ray().readwrite_unlock();
  store.FullIntersections().tri().readwrite_unlock();
  store.FullIntersections().t().readwrite_unlock();
  store.FullIntersections().barys().readwrite_unlock();
  
}


int main() {
  Store store  = Store::NewStore();
  CHECK_ERR();

  {
    MeshLoadOFF(store, "cube.off");
    printf("Loaded .off\n");
    RayLoadRFF(store, "simple.rff");
    printf("Loaded .rff\n");

    store.find_raytri_iscts_bool();
    printf("Intersected\n");
    MeshIsctPrintBoolIntersection(store);
    store.find_raytri_iscts();
    printf("Intersected\n");
    MeshIsctPrintRayTriIntersection(store);
    store.find_raytri_iscts_full();
    printf("Intersected\n");
    MeshIsctPrintFullIntersection(store);
  }

  std::cout << "raycaster done" << std::endl;
  return 0;
}
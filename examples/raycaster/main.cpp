
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

  MeshLoad( store, positions.size(), indices.size(), 
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
    tMax[i] = std::numeric_limits<float>::infinity();
  }

  RayLoad( store, tMax.size(), origins.data(), directions.data(), tMax.data() );
}


void MeshIsctPrint( Store store )
{
  
  uint32_t n_isct       = store.RTIntersections().size();
  printf("Num ray tri intersections: %u\n", n_isct);
  uint32_t* t           = store.RTIntersections().tri().readwrite_lock();
  uint32_t* r           = store.RTIntersections().ray().readwrite_lock();
  for(uint32_t k=0; k<n_isct; k++) {
    printf("%u: ray %u / tri %u\n", k, r[k], t[k]);
  }
  store.RTIntersections().ray().readwrite_unlock();
  store.RTIntersections().tri().readwrite_unlock();
  
}


int main() {
  Store store  = Store::NewStore();
  CHECK_ERR();

  {
    MeshLoadOFF(store, "cube.off");
    printf("Loaded .off\n");
    RayLoadRFF(store, "simple.rff");
    printf("Loaded .rff\n");
    store.find_raytri_iscts();
    printf("Intersected\n");
    MeshIsctPrint(store);
  }

  std::cout << "raycaster done" << std::endl;
  return 0;
}
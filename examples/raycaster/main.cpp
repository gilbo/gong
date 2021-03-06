
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
               float* pos, uint32_t* vert ) {
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
  auto raystore = store.Rays();
  raystore.beginload(n_rays);
    raystore.origin().load(reinterpret_cast<vec3*>(origin), 3*sizeof(float));
      raystore.direction().load(reinterpret_cast<vec3*>(direction), 3*sizeof(float));
      raystore.tMax().load(tMax, sizeof(float));
  raystore.endload();

  void* zeros = malloc(n_rays*sizeof(bool));

  std::vector<uint32_t> sequentialIndices;
  sequentialIndices.resize(n_rays);
  for (int i = 0; i < n_rays; ++i) sequentialIndices[i] = i;

  auto rayhitstore = store.RayHits();
  rayhitstore.beginload(n_rays);
    rayhitstore.ray().load(sequentialIndices.data(),sizeof(uint32_t));
    rayhitstore.hit().load((bool*)zeros,sizeof(bool));
  rayhitstore.endload();
  free(zeros);
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
  auto hStore = store.RayHits();
  uint32_t n_isct   = hStore.size();
  printf("Num ray tri intersections: %u\n", n_isct);
  uint32_t* r       = hStore.ray().read_lock();
  bool* h           = hStore.hit().read_lock();
  for(uint32_t k=0; k<n_isct; k++) {
    printf("%u: ray %u\n", r[k], (uint32_t)h[r[k]]);
  }
  hStore.ray().read_unlock();
  hStore.hit().read_unlock();
}

void MeshIsctPrintRayTriIntersection( Store store )
{
  auto iStore = store.RayTriIntersections();
  uint32_t n_isct       = iStore.size();
  printf("Num ray tri intersections: %u\n", n_isct);
  uint32_t* t           = iStore.tri().read_lock();
  uint32_t* r           = iStore.ray().read_lock();
  for(uint32_t k=0; k<n_isct; k++) {
    printf("%u: ray %u / tri %u\n", k, r[k], t[k]);
  }
  iStore.ray().read_unlock();
  iStore.tri().read_unlock();
  
}

void MeshIsctPrintFullIntersection( Store store )
{
  auto iStore = store.FullIntersections();
  uint32_t n_isct       = iStore.size();
  printf("Num ray tri intersections: %u\n", n_isct);
  uint32_t* r           = iStore.ray().read_lock();
  uint32_t* tri         = iStore.tri().read_lock();
  float* t              = iStore.t().read_lock();
  float* b              = (float*)iStore.barys().read_lock();
  for(uint32_t k=0; k<n_isct; k++) {
    printf("ray %u: %u / tri %u: %f (%f, %f, %f)\n", k, r[k], tri[k], t[k], b[k*3+0],b[k*3+1],b[k*3+2]);
  }
  iStore.ray().read_unlock();
  iStore.tri().read_unlock();
  iStore.t().read_unlock();
  iStore.barys().read_unlock();
  
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
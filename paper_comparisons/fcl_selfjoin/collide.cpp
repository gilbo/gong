#include "collide.h"
#include "timer.h"
#ifdef _WIN32
#pragma warning( push )
#pragma warning( disable : 4244)
#pragma warning( disable : 4065)
#pragma warning( disable : 4800)
#endif
#include "fcl/config.h"
#include "fcl/narrowphase/collision.h"
#include "fcl/narrowphase/collision_request.h"
#include "fcl/broadphase/broadphase_dynamic_AABB_tree.h"
#include "fcl/geometry/geometric_shape_to_BVH_model.h"

#ifdef USE_BVH
#include "collide_bvh.h"
#else
#include "collide_scan.h"
#endif
#ifdef _WIN32
#pragma warning( pop ) 
#endif
using namespace fcl;


#   define assertM(condition, message) \
    do { \
        if (! (condition)) { \
            std::cerr << "Assertion `" #condition "` failed in " << __FILE__ \
                      << " line " << __LINE__ << ": " << message << std::endl; \
            std::terminate(); \
        } \
    } while (false)


#define TINYPLY_IMPLEMENTATION
#include "tinyply.h"

#include <chrono>
#include <fstream>




static bool approxEqual(float x0, float x1, float epsilon = 1e-5) {
    return std::abs(x0-x1) < epsilon;
}

static bool approxEqual(vec3f v0, vec3f v1, float epsilon = 1e-5) {
    return approxEqual(v0.x,v1.x) && approxEqual(v0.y,v1.y) && approxEqual(v0.z,v1.z);
}

struct ComparisonContact {
    uint32_t obj0 = -1;
    uint32_t obj1 = -1;
    uint32_t tri0;
    uint32_t tri1;
    vec3f pos;
    float depth;
    vec3f nrml;
    ComparisonContact() {}
    ComparisonContact(uint32_t o0, uint32_t o1, uint32_t t0, uint32_t t1, vec3f p, float d, vec3f n) :
    obj0(o1), obj1(o1), tri0(t0), tri1(t1), pos(p), depth(d), nrml(n) {}
    ComparisonContact(fcl::Contact<float> c) : tri0(c.b1), tri1(c.b2), depth(c.penetration_depth) {
        pos = {c.pos.x(),c.pos.y(),c.pos.z()};
        nrml = {c.normal.x(),c.normal.y(),c.normal.z()};
    }
    bool operator==(const ComparisonContact& t) const
    { 
        return (
            ((this->tri0 == t.tri0 &&
            this->tri1 == t.tri1) ||
            (this->tri1 == t.tri0 &&
            this->tri0 == t.tri1)) &&
            approxEqual(this->pos, t.pos)); 
    } 
}; 
  
class ComparisonContactHashFunction { 
public: 
    // Only compare tri IDs for hash. Not a good hash function, but works in a pinch
    // And is better than doing a full linear scan...
    size_t operator()(const ComparisonContact& t) const
    { 
        return ((size_t)t.tri0*t.tri0*t.tri0) + ((size_t)t.tri1*t.tri1*t.tri1); 
    } 
}; 


/// @brief Collision data stores the collision request and the result given by collision algorithm.
template <typename S>
struct CollisionData
{
    CollisionData()
    {
        done = false;
    }

    /// @brief Collision request
    CollisionRequest<S> request;

    /// @brief Collision result
    CollisionResult<S> result;

    /// @brief Whether the collision iteration can stop
    bool done;
};

template <typename S>
bool defaultCollisionFunction(CollisionObject<S>* o1, CollisionObject<S>* o2, void* cdata_) {
    auto* cdata = static_cast<CollisionData<S>*>(cdata_);
    const auto& request = cdata->request;
    auto& result = cdata->result;

    if (cdata->done) return true;
    collide(o1, o2, request, result);
    if (!request.enable_cost && (result.isCollision()) && (result.numContacts() >= request.num_max_contacts))
        cdata->done = true;

    return cdata->done;
}

class manual_timer
{
    std::chrono::high_resolution_clock::time_point t0;
    double timestamp{ 0.f };
public:
    void start() { t0 = std::chrono::high_resolution_clock::now(); }
    void stop() { timestamp = std::chrono::duration<float>(std::chrono::high_resolution_clock::now() - t0).count() * 1000; }
    const double & get() { return timestamp; }
};


void readPly(const std::string & filepath, std::vector<Vector3f>& verts, std::vector<Triangle>& tris) {
    try {
        std::ifstream ss(filepath, std::ios::binary);
        if (ss.fail()) throw std::runtime_error("failed to open " + filepath);

        PlyFile file;
        file.parse_header(ss);
        /*
        std::cout << "........................................................................\n";
        for (auto c : file.get_comments()) std::cout << "Comment: " << c << std::endl;
        for (auto e : file.get_elements()) {
            std::cout << "element - " << e.name << " (" << e.size << ")" << std::endl;
            for (auto p : e.properties) std::cout << "\tproperty - " << p.name << " (" << tinyply::PropertyTable[p.propertyType].str << ")" << std::endl;
        }
        std::cout << "........................................................................\n";
        */
        // Tinyply treats parsed data as untyped byte buffers.
        std::shared_ptr<PlyData> vertices, faces;

        // The header information can be used to programmatically extract properties on elements
        // known to exist in the header prior to reading the data. For brevity of this sample, properties 
        // like vertex position are hard-coded: 
        try { vertices = file.request_properties_from_element("vertex", { "x", "y", "z" }); }
        catch (const std::exception & e) { std::cerr << "tinyply exception: " << e.what() << std::endl; }

        // Providing a list size hint (the last argument) is a 2x performance improvement. If you have 
        // arbitrary ply files, it is best to leave this 0. 
        try { faces = file.request_properties_from_element("face", { "vertex_indices" }, 3); }
        catch (const std::exception & e) { std::cerr << "tinyply exception: " << e.what() << std::endl; }

        manual_timer read_timer;

        read_timer.start();
        file.read(ss);
        read_timer.stop();

        //std::cout << "Reading took " << read_timer.get() / 1000.f << " seconds." << std::endl;
        //if (vertices) std::cout << "\tRead " << vertices->count << " total vertices " << std::endl;
        //if (faces) std::cout << "\tRead " << faces->count << " total faces (triangles) " << std::endl;

        {
            const size_t numVerticesBytes = vertices->buffer.size_bytes();
            verts.resize(vertices->count);
            std::memcpy(verts.data(), vertices->buffer.get(), numVerticesBytes);
            if (vertices->t != tinyply::Type::FLOAT32) {
                throw std::runtime_error("verts were not floats in " + filepath);
            }
            std::vector<uint32> indices;
            if (PropertyTable[faces->t].stride != 4) {
                throw std::runtime_error("indices were not 4 bytes in " + filepath);
            }
            tris.resize(faces->count);
            //printf("Vert 0 == (%g %g %g)\n", verts[0].x(), verts[0].y(), verts[0].z());
            indices.resize(faces->count * 3);
            std::memcpy(indices.data(), faces->buffer.get(), faces->buffer.size_bytes());
            for (int f = 0; f < tris.size(); ++f) {
                tris[f].set((size_t)indices[f * 3 + 0], (size_t)indices[f * 3 + 1], (size_t)indices[f * 3 + 2]);
            }
        }

    } catch (const std::exception & e) {
        std::cerr << "Caught tinyply exception: " << e.what() << std::endl;
    }
}

void writePly(const std::string & filename, const std::vector<Vector3f>& verts, const std::vector<Triangle>& tris) {
    std::filebuf fb_ascii;
    fb_ascii.open(filename, std::ios::out);
    std::ostream outstream_ascii(&fb_ascii);
    if (outstream_ascii.fail()) throw std::runtime_error("failed to open " + filename);

    PlyFile plyFile;

    plyFile.add_properties_to_element("vertex", { "x", "y", "z" },
        Type::FLOAT32, verts.size(), reinterpret_cast<uint8_t*>(const_cast<Vector3f*>(verts.data())), Type::INVALID, 0);

    std::vector<uint32> indices;
    indices.resize(3 * tris.size());
    for (int t = 0; t < tris.size(); ++t) {
        for (int i = 0; i < 3; ++i) indices[3 * t + i] = (uint32)tris[t][i];
    }
    if (indices.size() > 0) {
        plyFile.add_properties_to_element("face", { "vertex_indices" },
            Type::UINT32, tris.size(), reinterpret_cast<uint8_t*>(indices.data()), Type::UINT8, 3);
    }

    plyFile.get_comments().push_back("generated by tinyply 2.2");

    plyFile.write(outstream_ascii, false);
}


void splitIntoComponents(const std::vector<Vector3f>& verts, const std::vector<Triangle>& tris,
    const std::vector<int>& vertexStartLocations,
    std::vector<std::vector<Vector3f>>& outVerts, std::vector<std::vector<Triangle>>& outTris) {
    size_t numComponents = vertexStartLocations.size();
    outVerts.resize(numComponents);
    outTris.resize(numComponents);
    int startLocation = -1;
    for (int i = 0; i < numComponents; ++i) {
        assertM(startLocation < vertexStartLocations[i], "Start locations must be in ascending order");
        startLocation = vertexStartLocations[i];
        int endLocation = (i + 1 < numComponents) ? vertexStartLocations[i + 1] : (int)verts.size();
        int componentVertCount = endLocation - startLocation;
        outVerts[i].resize(componentVertCount);
        memcpy(outVerts[i].data(), verts.data() + startLocation, componentVertCount * sizeof(Vector3f));
        //printf("Vert 0 == (%g %g %g)\n", outVerts[i][0].x(), outVerts[i][0].y(), outVerts[i][0].z());
        auto inRange = [&](size_t index) {
            return (index >= startLocation) && (index < endLocation);
        };
        outTris[i].clear();
        outTris[i].reserve(tris.size());
        for (int t = 0; t < tris.size(); ++t) {
            Triangle T = tris[t];
            if (inRange(T[0])) {
                assertM(inRange(T[1]) && inRange(T[2]), "Triangle spans multiple components!");
                Triangle newT(T[0] - startLocation, T[1] - startLocation, T[2] - startLocation);
                outTris[i].push_back(newT);
            }
        }
    }
}



void splitIntoConnectedComponents(const std::vector<Vector3f>& verts, const std::vector<Triangle>& tris,
    std::vector<std::vector<Vector3f>>& outVerts, std::vector<std::vector<Triangle>>& outTris, std::vector<uint>& outObjIDs, std::vector<int>& startTriIDs) {

    std::vector<std::vector<int>> adjacency;
    adjacency.resize(verts.size());
    for (auto t : tris) {
        for (int i = 0; i < 3; ++i) {
            int j = (i + 1) % 3;
            adjacency[t[i]].push_back((int)t[j]);
            adjacency[t[j]].push_back((int)t[i]);
        }
    }
    // A search that begins at some particular vertex v will find the entire component containing v 
    // (and no more) before returning. To find all the components of a graph, loop through its vertices, 
    // starting a new breadth first or depth first search whenever the loop reaches a vertex that has 
    // not already been included in a previously found component.
    std::vector<std::unordered_set<int>> components;
    std::vector<bool> foundComponent;
    foundComponent.resize(verts.size());
    for (int i = 0; i < verts.size(); ++i) {
        foundComponent[i] = false;
    }
    std::function<void(int, std::unordered_set<int>&)> dfs = 
        [&adjacency,&foundComponent,&dfs](int vert, std::unordered_set<int>& component) {
            if (component.count(vert) == 1) return;
            component.insert(vert);
            foundComponent[vert] = true;
            const std::vector<int>& neighbors = adjacency[vert];
            for (int i = 0; i < neighbors.size(); ++i) {
                dfs(neighbors[i], component);
            }
    };

    std::vector<int> vertexStartIDs;
    // We're going to assume that vertices for a connected component are all in one block;
    // Then we can build vertexStartLocations as we go along, and leverage code we already wrote
    // to break it apart
    for (int i = 0; i < verts.size(); ++i) {
        if (!foundComponent[i]) {
            components.push_back(std::unordered_set<int>());
            dfs(i, components[components.size() - 1]);
            vertexStartIDs.push_back(i);
        }
    }

    // Now `components` contains the connected components in the form of unset ints
    splitIntoComponents(verts, tris, vertexStartIDs, outVerts, outTris);

    std::vector<int> vertToObjID;

    // Create an object ID map.
    for (int i = 0; i < vertexStartIDs.size(); ++i) {
        int start = vertexStartIDs[i];
        int end = verts.size();
        if (i < vertexStartIDs.size() - 1) {
            end = vertexStartIDs[i+1];
        }
        for (int j = start; j < end; ++j) {
            vertToObjID.push_back(i);
        }

    }

    int lastObjID = -1;
    for (int i = 0; i < tris.size(); ++i) {
        // Triangles have all vertices in same component, so just check one.
        auto v = tris[i][0];
        int newID = vertToObjID[v];
        if (newID != lastObjID) {
            startTriIDs.push_back(i);
        }
        outObjIDs.push_back(newID);
        lastObjID = newID;

    }

/*
    printf("Out IDs ****\n");
    for (int i = 0; i < outObjIDs.size(); ++i) {
        printf("%d: %d\n", i, outObjIDs[i]);
    }
    printf("************\n");
*/
    
}

void createGeometries(std::string filename, const std::vector<int>& componentStarts,
    std::vector<std::shared_ptr<BVHModel<OBBRSSf>>>& geometries) {

    // set mesh triangles and vertice indices
    std::vector<Vector3f> vertices;
    std::vector<Triangle> triangles;

    readPly(filename, vertices, triangles);

    std::vector<std::vector<Vector3f>> splitVerts;
    std::vector<std::vector<Triangle>> splitTris;
    splitIntoComponents(vertices, triangles, componentStarts, splitVerts, splitTris);
    geometries.clear();
    for (int i = 0; i < splitVerts.size(); ++i) {
        char componentFilename[20];
        sprintf(componentFilename, "component%d.ply", i);
        writePly(componentFilename, splitVerts[i], splitTris[i]);

        // BVHModel is a template class for mesh geometry, for default OBBRSS template is used
        auto geom = std::make_shared<BVHModel<OBBRSSf>>();
        // add the mesh data into the BVHModel structure
        geom->beginModel();
        geom->addSubModel(splitVerts[i], splitTris[i]);
        geom->endModel();
        geometries.push_back(geom);
    }
}


void placeComponentsIntoTree(const std::vector<std::vector<Vector3f>>& splitVerts, const std::vector<std::vector<Triangle>>& splitTris, BroadPhaseCollisionManagerf* manager) {
    Transform3f pose = Transform3f::Identity();
    // set mesh triangles and vertice indices
    for (int i = splitVerts.size()-1; i >= 0; --i) {
        char componentFilename[20];
        sprintf(componentFilename, "component%d.ply", i);
        //writePly(componentFilename, splitVerts[i], splitTris[i]);

        // BVHModel is a template class for mesh geometry, for default OBBRSS template is used
        auto geom = std::make_shared<BVHModel<OBBRSSf>>();
        // add the mesh data into the BVHModel structure
        geom->beginModel();
        geom->addSubModel(splitVerts[i], splitTris[i]); 
        geom->endModel();
        CollisionObjectf* obj0 = new CollisionObjectf(geom, pose);
        manager->registerObject(obj0);
    }
    // Setup the managers, which is related with initializing the broadphase acceleration structure according to objects input
    manager->setup();
}


void runFCLCollisionOnComponents(const std::vector<std::vector<Vector3f>>& splitVerts, const std::vector<std::vector<Triangle>>& splitTris, std::vector<ComparisonContact>& fclContacts) {
    // Generally, the DynamicAABBTreeCollisionManager would provide the best performance.
    BroadPhaseCollisionManagerf* manager = new DynamicAABBTreeCollisionManagerf();
    placeComponentsIntoTree(splitVerts,splitTris,manager);
    printf("Connected Component Count: %zd\n", splitVerts.size());
    CollisionData<float> collision_data;
    collision_data.request.num_max_contacts = 30000;
    collision_data.request.enable_contact = true;
    // Self collision query
    double before = GetCurrentTimeInSeconds();
    manager->collide(&collision_data, defaultCollisionFunction);
    double collisionTime = GetCurrentTimeInSeconds() - before;
    printf("FCL Collision Time: %g ms\n", collisionTime*1000.0);
    printf("FCL Component Collision Num Contacts: %zd\n", collision_data.result.numContacts());
    if (collision_data.result.numContacts() > 0) {
        std::vector<fcl::Contact<float>> contacts;
        collision_data.result.getContacts(contacts);
        std::vector<Vector3f> vertices;
        vertices.resize(contacts.size());
        for (int i = 0; i < contacts.size(); ++i) {
            auto& c = contacts[i];
            vertices[i] = c.pos;
            /*
            printf("(?: %d) vs (?: %d): %f (%f %f %f); (%f %f %f)\n", c.b1, 
                c.b2, c.penetration_depth, c.pos.x(),c.pos.y(),c.pos.z(),
                c.normal.x(),c.normal.y(),c.normal.z());
            */
            fclContacts.push_back(ComparisonContact(c));
        }
        writePly("fcl_contacts.ply", vertices, {});
    }
}

#define CHECK_ERR() { \
  if(store.geterror()) { \
    std::cerr << store.geterror() << std::endl; \
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
               float* pos, uint32_t* vert, uint32_t* obj_id )
{
  // build a set of edges
  std::set< std::pair<uint32_t,uint32_t> >  edges;
  for(uint32_t i=0; i<n_tri; i++) {
    uint32_t v0;
    uint32_t v1;
    v0            = vert[3*i+0];
    v1            = vert[3*i+1];
    if(v1 < v0) { std::swap(v0,v1); }
    edges.insert(std::make_pair(v0, v1));
    v0            = vert[3*i+0];
    v1            = vert[3*i+2];
    if(v1 < v0) { std::swap(v0,v1); }
    edges.insert(std::make_pair(v0, v1));
    v0            = vert[3*i+1];
    v1            = vert[3*i+2];
    if(v1 < v0) { swap(v0,v1); }
    edges.insert(std::make_pair(v0, v1));
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
  printf("V/E/T: %u/%u/%u\n", n_vert, n_edge, n_tri);
  store.Verts().beginload(n_vert);
  store.Edges().beginload(n_edge);
  store.Tris().beginload(n_tri);
    store.Verts().pos().load(reinterpret_cast<vec3*>(pos), 3*sizeof(float));
    store.Edges().hd().load(e_hd.data(), sizeof(uint32_t));
    store.Edges().tl().load(e_tl.data(), sizeof(uint32_t));
    store.Tris().v().load(reinterpret_cast<tri3*>(vert), 3*sizeof(uint32_t));
    store.Tris().obj_id().load(obj_id, sizeof(uint32_t));
  store.Verts().endload();
  store.Edges().endload();
  store.Tris().endload();
  /*
  printf("Tris\n");
  for (int i = 0; i < n_tri; ++i) {
    printf("%d: %u %u %u\n", i, vert[3*i],vert[3*i+1],vert[3*i+2]);
  }
  printf("Verts\n");
  for (int i = 0; i < n_vert; ++i) {
    printf("%d: %f %f %f\n", i, pos[3*i],pos[3*i+1],pos[3*i+2]);
  }
  */
}

inline std::ostream&
operator<<(std::ostream &out, vec3 o) {
  out << "(" << o.d[0] << "," << o.d[1] << "," << o.d[2] << ")";
  return out;
}



void MeshIsctRead(Store store, const std::vector<int>& startIDs, std::vector<ComparisonContact>& contacts) {
    uint32_t n_isct     = store.TTcontacts().size();
    std::cout << "**Gong: Found " << n_isct << " triangle-triangle intersections." << endl;

    uint32_t* o0          = store.TTcontacts().obj0().readwrite_lock();
    uint32_t* o1          = store.TTcontacts().obj1().readwrite_lock();
    uint32_t* t0          = store.TTcontacts().tri0().readwrite_lock();
    uint32_t* t1          = store.TTcontacts().tri1().readwrite_lock();
    vec3*     pos         = store.TTcontacts().pos().readwrite_lock();
    float*    depth       = store.TTcontacts().penetration_depth().readwrite_lock();
    vec3*     nrml        = store.TTcontacts().normal().readwrite_lock();

    std::vector<Vector3f> vertices;
    vertices.resize(n_isct);
    for(uint32_t k=0; k<n_isct; k++) {
        uint obj0 = o0[k];
        uint obj1 = o1[k];
        uint o0Off = (uint)startIDs[obj0];
        uint o1Off = (uint)startIDs[obj1];
        vec3f p = {pos[k].d[0],pos[k].d[1],pos[k].d[2]};
        vec3f n = {nrml[k].d[0],nrml[k].d[1],nrml[k].d[2]};
        ComparisonContact c(obj0, obj1, t0[k]-o0Off, t1[k]-o1Off, p, depth[k], n);
        vertices[k] = {p.x,p.y,p.z};
        contacts.push_back(c);
    }
    writePly("gong_contacts.ply", vertices, {});
    //...
    store.TTcontacts().obj0().readwrite_unlock();
    store.TTcontacts().obj1().readwrite_unlock();
    store.TTcontacts().tri0().readwrite_unlock();
    store.TTcontacts().tri1().readwrite_unlock();
    store.TTcontacts().pos().readwrite_unlock();
    store.TTcontacts().penetration_depth().readwrite_unlock();
    store.TTcontacts().normal().readwrite_unlock();
}


void runGongCollisionOnComponents(const std::vector<Vector3f>& vertices, const std::vector<Triangle>& tris, const std::vector<uint>& objIDs, const std::vector<int>& startIDs, std::vector<ComparisonContact>& contacts) {
  printf("V/T/ObjIDs = %zd, %zd, %zd\n", vertices.size(), tris.size(), objIDs.size());

  std::vector<uint32_t> smallTris;
  for (auto t : tris) {
    smallTris.push_back((uint32_t)t[0]);
    smallTris.push_back((uint32_t)t[1]);
    smallTris.push_back((uint32_t)t[2]);
  }


  Store store  = Store::NewStore();
  CHECK_ERR();
  {
    uint32_t n_vert = (uint32_t)vertices.size();
    uint32_t n_tri = (uint32_t)tris.size();
    float* pos = (float*)vertices.data();
    uint32_t* vert = smallTris.data();
    uint32_t* obj_id = (uint32_t*)objIDs.data();
    MeshLoad( store, n_vert, n_tri, pos, vert, obj_id );

    double before = GetCurrentTimeInSeconds();
    store.find_tt_iscts();
    double collisionTime = GetCurrentTimeInSeconds() - before;
    printf("gong Collision Time: %g ms\n", collisionTime*1000.0);

    MeshIsctRead(store, startIDs, contacts);
  }
}

void runCollisionTestOnConnectedComponents(std::string filename) {
    std::vector<Vector3f> vertices;
    std::vector<Triangle> triangles;
    readPly(filename, vertices, triangles);
    std::vector<std::vector<Vector3f>> splitVerts;
    std::vector<std::vector<Triangle>> splitTris;
    std::vector<uint> objIDsPerTriangle;
    std::vector<int> startIDs;
    splitIntoConnectedComponents(vertices, triangles, splitVerts, splitTris, objIDsPerTriangle, startIDs);

    { // Collide and compare 
        std::vector<ComparisonContact> fclContacts;
        std::vector<ComparisonContact> gongContacts;
        runFCLCollisionOnComponents(splitVerts,splitTris, fclContacts);
        runGongCollisionOnComponents(vertices,triangles,objIDsPerTriangle,startIDs, gongContacts);
        std::unordered_set<ComparisonContact, ComparisonContactHashFunction> gongSet(gongContacts.begin(), gongContacts.end());
        int missingContactCount = 0;
        for (auto& c : fclContacts) {
            auto search = gongSet.find(c);
            if (search == gongSet.end()) {
                ++missingContactCount;
                printf("Missing Contact in Gong Set: %d/%d at (%f %f %f), depth %f, normal (%f %f %f)\n", 
                    c.tri0, c.tri1, c.pos.x, c.pos.y, c.pos.z, c.depth, c.nrml.x, c.nrml.y, c.nrml.z);
            }
        }
        printf("Missing Contact in Gong Count: %d\n", missingContactCount);

    }
}

void runCollisionTest(std::string filename, const std::vector<int>& componentStarts, vec3f gravStep) {

    BroadPhaseCollisionManagerf* manager = new DynamicAABBTreeCollisionManagerf();

    std::vector<std::shared_ptr<BVHModel<OBBRSSf>>> geometries;
    createGeometries(filename, componentStarts, geometries);

    // R and T are the rotation matrix and translation vector
    Matrix3f R = Matrix3f::Identity();
    Vector3f T = { gravStep.x, gravStep.y, gravStep.z };
    if (geometries.size() == 2) {
        // transform is configured according to R and T
        Transform3f pose = Transform3f::Identity();
        Transform3f pose2 = Transform3f::Identity();

        //Combine them together
        CollisionObjectf* obj0 = new CollisionObjectf(geometries[0], pose);
        CollisionObjectf* obj1 = new CollisionObjectf(geometries[1], pose2);

        // Generally, the DynamicAABBTreeCollisionManager would provide the best performance.
        BroadPhaseCollisionManagerf* manager0 = new DynamicAABBTreeCollisionManagerf();
        // Initialize the collision manager for the second group of objects.
        BroadPhaseCollisionManagerf* manager1 = new DynamicAABBTreeCollisionManagerf();
        manager0->registerObject(obj0);
        manager1->registerObject(obj1);

        // Setup the managers, which is related with initializing the broadphase acceleration structure according to objects input
        manager0->setup();
        manager1->setup();

        CollisionData<float> self0_data, self1_data, collision_data, simple_collision;
        collision_data.request.num_max_contacts = 10000;
        simple_collision.request.num_max_contacts = 10000;
        // Self collision query

        manager0->collide(manager1, &collision_data, defaultCollisionFunction);
        collide(obj0, obj1, simple_collision.request, simple_collision.result);
        if (collision_data.result.numContacts() > 0) {
            printf("Num Contacts: %zd\n", collision_data.result.numContacts());
        }
    } else {
        Matrix3f R = Matrix3f::Identity();
        Vector3f T = { gravStep.x, gravStep.y, gravStep.z };
        Transform3f pose = Transform3f::Identity();
        pose.linear() = R;
        pose.translation() = Vector3f{ 0.0f,0.0f,0.0f };
        //Combine them together
        CollisionObjectf* obj0 = new CollisionObjectf(geometries[0], pose);
        // Generally, the DynamicAABBTreeCollisionManager would provide the best performance.
        BroadPhaseCollisionManagerf* manager0 = new DynamicAABBTreeCollisionManagerf();
        manager0->registerObject(obj0);

        // Setup the managers, which is related with initializing the broadphase acceleration structure according to objects input
        manager0->setup();

        CollisionData<float> collision_data;
        collision_data.request.num_max_contacts = 10000;
        // Self collision query

        manager0->collide(&collision_data, defaultCollisionFunction);
        if (collision_data.result.numContacts() > 0) {
            printf("Self-Collision Num Contacts: %zd\n", collision_data.result.numContacts());
        }
    }
}

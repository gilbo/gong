#include "collide.h"

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
    std::vector<std::vector<Vector3f>>& outVerts, std::vector<std::vector<Triangle>>& outTris) {

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

    // We're going to assume that vertices for a connected component are all in one block;
    // Then we can build vertexStartLocations as we go along, and leverage code we already wrote
    // to break it apart
    std::vector<int> vertexStartLocations;
    for (int i = 0; i < verts.size(); ++i) {
        if (!foundComponent[i]) {
            components.push_back(std::unordered_set<int>());
            dfs(i, components[components.size() - 1]);
            vertexStartLocations.push_back(i);
        }
    }

    // Now `components` contains the connected components in the form of unset ints
    splitIntoComponents(verts, tris, vertexStartLocations, outVerts, outTris);
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

void placeComponentsIntoTree(std::string filename, BroadPhaseCollisionManagerf* manager) {
    Transform3f pose = Transform3f::Identity();
    // set mesh triangles and vertice indices
    std::vector<Vector3f> vertices;
    std::vector<Triangle> triangles;
    readPly(filename, vertices, triangles);
    std::vector<std::vector<Vector3f>> splitVerts;
    std::vector<std::vector<Triangle>> splitTris;
    splitIntoConnectedComponents(vertices, triangles,splitVerts,splitTris);
    printf("Connected Component Count: %zd\n", splitVerts.size());

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
        CollisionObjectf* obj0 = new CollisionObjectf(geom, pose);
        manager->registerObject(obj0);
    }
    // Setup the managers, which is related with initializing the broadphase acceleration structure according to objects input
    manager->setup();
}


void runCollisionTestOnConnectedComponents(std::string filename) {
    // Generally, the DynamicAABBTreeCollisionManager would provide the best performance.
    BroadPhaseCollisionManagerf* manager = new DynamicAABBTreeCollisionManagerf();
    placeComponentsIntoTree(filename, manager);
    CollisionData<float> collision_data;
    collision_data.request.num_max_contacts = 30000;
    collision_data.request.enable_contact = true;
    // Self collision query

    manager->collide(&collision_data, defaultCollisionFunction);
    if (collision_data.result.numContacts() > 0) {
        printf("Self-Collision Num Contacts: %zd\n", collision_data.result.numContacts());
        std::vector<fcl::Contact<float>> contacts;
        collision_data.result.getContacts(contacts);
        std::vector<Vector3f> vertices;
        vertices.resize(contacts.size());
        for (int i = 0; i < contacts.size(); ++i) {
            vertices[i] = contacts[i].pos;
            //printf("Contact %d: %d/%d (%f %f %f)\n", contacts[i].b1, contacts[])
        }
        writePly("contacts.ply", vertices, {});
    }
}


void runCollisionTest(std::string filename, const std::vector<int>& componentStarts, vec3 gravStep) {

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
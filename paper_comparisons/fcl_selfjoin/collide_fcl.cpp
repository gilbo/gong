#include "collide.h"
#include "timer.h"
#include "mesh_io.h"
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

void readPly(const std::string & filepath, std::vector<Vector3<Float>>& verts, std::vector<Triangle>& tris);

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


static ComparisonContact toComparisonContact(const fcl::Contact<Float>& c, GeometryIDMap& idMap) {
	ComparisonContact cc;
	cc.tri0 = c.b1;
	cc.tri1 = c.b2;
	cc.depth = c.penetration_depth;
	cc.pos = { c.pos.x(),c.pos.y(),c.pos.z() };
	cc.nrml = { c.normal.x(),c.normal.y(),c.normal.z() };
	cc.obj0 = idMap[(size_t)c.o1];
	cc.obj1 = idMap[(size_t)c.o2];
	cc.canonicalize();
	return cc;
}


void createGeometries(std::string filename, const std::vector<int>& componentStarts,
	std::vector<std::shared_ptr<BVHModel<OBBRSSf>>>& geometries) {

	// set mesh triangles and vertice indices
	std::vector<Vector3<Float>> vertices;
	std::vector<Triangle> triangles;

	readPly(filename, vertices, triangles);

	std::vector<std::vector<Vector3<Float>>> splitVerts;
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


void placeComponentsIntoTree(const std::vector<std::vector<Vector3<Float>>>& splitVerts, const std::vector<std::vector<Triangle>>& splitTris, BroadPhaseCollisionManagerf* manager, GeometryIDMap& geomToIndex) {
	std::vector<CollisionGeometry<Float>*> fclObjects;
	fclObjects.resize(splitVerts.size());
	double before = GetCurrentTimeInSeconds();
	Transform3f pose = Transform3f::Identity();
	// set mesh triangles and vertice indices
	for (int i = 0; i < (int)splitVerts.size(); ++i) {
		// BVHModel is a template class for mesh geometry, for default OBBRSS template is used
		auto geom = std::make_shared<BVHModel<OBBRSSf>>();
		// add the mesh data into the BVHModel structure
		geom->beginModel();
		geom->addSubModel(splitVerts[i], splitTris[i]);
		geom->endModel();
		CollisionObjectf* obj = new CollisionObjectf(geom, pose);
		manager->registerObject(obj);
		fclObjects[i] = geom.get();
	}
	// Setup the managers, which is related with initializing the broadphase acceleration structure according to objects input
	manager->setup();
	double collisionTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Acceleration Build: %g ms\n", collisionTime*1000.0);
	for (int i = 0; i < fclObjects.size(); ++i) {
		geomToIndex[(size_t)fclObjects[i]] = i;
	}
}


void runFCLCollisionOnComponents(const std::vector<std::vector<Vector3<Float>>>& splitVerts, const std::vector<std::vector<Triangle>>& splitTris, std::vector<ComparisonContact>& fclContacts) {
	// Generally, the DynamicAABBTreeCollisionManager would provide the best performance.
	BroadPhaseCollisionManagerf* manager = new DynamicAABBTreeCollisionManagerf();
	GeometryIDMap idMap;
	placeComponentsIntoTree(splitVerts, splitTris, manager, idMap);
	printf("Connected Component Count: %zd\n", splitVerts.size());
	CollisionData<Float> collision_data;
	collision_data.request.num_max_contacts = 30000;
	collision_data.request.enable_contact = true;
	// Self collision query
	double before = GetCurrentTimeInSeconds();
	manager->collide(&collision_data, defaultCollisionFunction);
	double collisionTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Collision Time: %g ms\n", collisionTime*1000.0);
	printf("FCL Component Collision Num Contacts: %zd\n", collision_data.result.numContacts());
	if (collision_data.result.numContacts() > 0) {
		std::vector<fcl::Contact<Float>> contacts;
		collision_data.result.getContacts(contacts);
		std::vector<Vector3<Float>> vertices;
		vertices.resize(contacts.size());
		for (int i = 0; i < contacts.size(); ++i) {
			auto& c = contacts[i];
			vertices[i] = c.pos;
			/*
			printf("(?: %d) vs (?: %d): %f (%f %f %f); (%f %f %f)\n", c.b1,
			c.b2, c.penetration_depth, c.pos.x(),c.pos.y(),c.pos.z(),
			c.normal.x(),c.normal.y(),c.normal.z());
			*/
			fclContacts.push_back(toComparisonContact(c, idMap));
		}
		writePly("fcl_contacts.ply", vertices, {});
	}
}
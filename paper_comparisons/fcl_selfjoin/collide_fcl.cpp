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
#include "fcl/broadphase/broadphase_bruteforce.h"
#include "fcl/broadphase/broadphase_spatialhash.h"
#include "fcl/broadphase/broadphase_SaP.h"
#include "fcl/broadphase/broadphase_SSaP.h"
#include "fcl/broadphase/broadphase_interval_tree.h"
#include "fcl/broadphase/broadphase_dynamic_AABB_tree.h"
#include "fcl/broadphase/broadphase_dynamic_AABB_tree_array.h"
#include "fcl/broadphase/detail/sparse_hash_table.h"
#include "fcl/broadphase/detail/spatial_hash.h"
#include "fcl/geometry/geometric_shape_to_BVH_model.h"
#include "fcl/narrowphase/detail/traversal/collision_node.h"
typedef Float S;
#ifdef _WIN32
#pragma warning( pop ) 
#endif
using namespace fcl;

#if BVHNodeType == 0
typedef OBBRSSf BVHBounds;
#elif BVHNodeType == 1
typedef AABBf BVHBounds;
#elif BVHNodeType == 2
typedef OBB<float> BVHBounds;
#elif BVHNodeType == 3
typedef RSS<float> BVHBounds;
#elif BVHNodeType == 4
typedef kIOS<float> BVHBounds;
#elif BVHNodeType == 5
typedef KDOP<float, 16> BVHBounds;
#elif BVHNodeType == 6
typedef KDOP<float, 18> BVHBounds;
#elif BVHNodeType == 7
typedef KDOP<float, 24> BVHBounds;
#else
#error "Bad BVHNodeType"
#endif

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


struct FCLState {
	BroadPhaseCollisionManagerf* manager;
	std::vector<CollisionObject<Float>*> objects;
	GeometryIDMap idMap;
};

double testFCLFlattened(const std::vector<Vector3<Float>>& verts, std::vector<Triangle>& tris) {

  auto manager = new NaiveCollisionManager<Float>();

  Transform3f pose = Transform3f::Identity();
	// set mesh triangles and vertice indices
  CollisionObjectf* obj;
  std::shared_ptr<BVHModel<BVHBounds>> o;
	double before = GetCurrentTimeInSeconds();
  for (int i = 0; i < 1; ++i) {
		// BVHModel is a template class for mesh geometry, for default OBBRSS template is used
		o = std::make_shared<BVHModel<BVHBounds>>();
		// add the mesh data into the BVHModel structure
		o->beginModel();
		o->addSubModel(verts, tris);
		o->endModel();
		obj = new CollisionObjectf(o, pose);
		manager->registerObject(obj);
		//		fclObjects[i] = obj;
	}
manager->setup();
	double buildTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Flattened Build Time: %g ms\n", buildTime*1000.0);


	before = GetCurrentTimeInSeconds();
	{
		o->beginReplaceModel();
		o->replaceSubModel(verts);
		o->endReplaceModel();
		o->computeLocalAABB();
		obj->computeAABB();
	}
	double updateTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Flattened Update Position Time: %g ms\n", updateTime*1000.0);

	before = GetCurrentTimeInSeconds();
	{
		o->beginReplaceModel();
		o->replaceSubModel(verts);
		o->endReplaceModel(true,false);
		o->computeLocalAABB();
		obj->computeAABB();
	}
	double updateTime2 = GetCurrentTimeInSeconds() - before;
	printf("FCL Flattened Update Top-Down Position Time: %g ms\n", updateTime2*1000.0);

	
	CollisionData<Float> collision_data;
	collision_data.request.num_max_contacts = 3000000;
	collision_data.request.enable_contact = true;
	// Self collision query
	before = GetCurrentTimeInSeconds();

	detail::BVHFrontList front_list;

	detail::MeshCollisionTraversalNode<BVHBounds> node;
	if(!detail::initialize<BVHBounds>(node, *o, pose, *o, pose,collision_data.request, collision_data.result))
	  std::cout << "initialize error" << std::endl;

	selfCollide(&node, &front_list);

	double collisionTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Flattened Collision Time: %g ms\n", collisionTime*1000.0);
	printf("FCL Component Collision Num Contacts: %zd\n", collision_data.result.numContacts());
	return collisionTime + buildTime;
}


void initializeDataForFCL(const std::vector<std::vector<Vector3<Float>>>& splitVerts, const std::vector<std::vector<Triangle>>& splitTris, BroadPhaseCollisionManagerf* manager, std::vector<CollisionObject<Float>*>& fclObjects, GeometryIDMap& geomToIndex) {
	fclObjects.resize(splitVerts.size());
	Transform3f pose = Transform3f::Identity();
	// set mesh triangles and vertice indices
	for (int i = 0; i < (int)splitVerts.size(); ++i) {
		// BVHModel is a template class for mesh geometry, for default OBBRSS template is used
		auto geom = std::make_shared<BVHModel<BVHBounds>>();
		// add the mesh data into the BVHModel structure
		geom->beginModel();
		geom->addSubModel(splitVerts[i], splitTris[i]);
		geom->endModel();
		CollisionObjectf* obj = new CollisionObjectf(geom, pose);
		manager->registerObject(obj);
		fclObjects[i] = obj;
	}
	for (int i = 0; i < fclObjects.size(); ++i) {
		geomToIndex[(size_t)fclObjects[i]] = i;
	}
}

#ifndef COLLISION_MANAGER_TYPE
#define COLLISION_MANAGER_TYPE 0
#endif



FCLState* initializeFCL(const std::vector<std::vector<Vector3<Float>>>& splitVerts, 
		const std::vector<std::vector<Triangle>>& splitTris, 
		int managerIndex) {
	FCLState* state = new FCLState;
	switch(managerIndex) {
	case 0:
		state->manager = new NaiveCollisionManager<S>();
		break;
 	case 1:
 		state->manager = new SSaPCollisionManager<S>();
 		break;
  	case 2:
  		state->manager = new SaPCollisionManager<S>();
  		break;
  	case 3:
  		state->manager = new IntervalTreeCollisionManager<S>();
  		break;
  	case 4:
  		state->manager = new DynamicAABBTreeCollisionManager<S>();
  		break;
  	case 5:
  		state->manager = new DynamicAABBTreeCollisionManager_Array<S>();
  		break;
	default:
	  printf("ERROR, INVALID COLLISION MANAGER TYPE");
	  exit(0);
	}

	initializeDataForFCL(splitVerts, splitTris, state->manager, state->objects, state->idMap);
	return state;
}

double updateFCLPositions(FCLState* state, const std::vector<std::vector<Vector3<Float>>>& splitVerts) {
	double before = GetCurrentTimeInSeconds();
	for (int i = 0; i < state->objects.size(); ++i) {
		auto o = (BVHModel<BVHBounds>*)state->objects[i]->collisionGeometry().get();
		o->beginReplaceModel();
		o->replaceSubModel(splitVerts[i]);
		o->endReplaceModel(true,false);
		o->computeLocalAABB();
		state->objects[i]->computeAABB();
	}
	double updateTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Update Position Time: %g ms\n", updateTime*1000.0);
	return updateTime;
}

double buildFCLAccelerationStructure(FCLState* state) {
	double before = GetCurrentTimeInSeconds();
	// Setup the managers, which is related with initializing the broadphase acceleration structure according to objects input
	state->manager->setup();
	double buildTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Acceleration Build: %g ms\n", buildTime*1000.0);
	return buildTime;
}

double refitFCLAccelerationStructure(FCLState* state) {
	double before = GetCurrentTimeInSeconds();
	// Setup the managers, which is related with initializing the broadphase acceleration structure according to objects input
	state->manager->update();
	double refitTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Acceleration Refit: %g ms\n", refitTime*1000.0);
	return refitTime;
}

double fclCollision(FCLState* state, std::vector<ComparisonContact>& fclContacts) {
	CollisionData<Float> collision_data;
	collision_data.request.num_max_contacts = 30000;
	collision_data.request.enable_contact = true;
	// Self collision query
	double before = GetCurrentTimeInSeconds();
	state->manager->collide(&collision_data, defaultCollisionFunction);
	double collisionTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Collision Time: %g ms\n", collisionTime*1000.0);
	printf("FCL Component Collision Num Contacts: %zd\n", collision_data.result.numContacts());
	fclContacts.clear();
	if (collision_data.result.numContacts() > 0) {
		std::vector<fcl::Contact<Float>> contacts;
		collision_data.result.getContacts(contacts);
		std::vector<Vector3<Float>> vertices;
		vertices.resize(contacts.size());
		for (int i = 0; i < contacts.size(); ++i) {
			auto& c = contacts[i];
			vertices[i] = c.pos;
			fclContacts.push_back(toComparisonContact(c, state->idMap));
		}
		writePly("fcl_contacts.ply", vertices, {});
	}
	return collisionTime;
}

void fclCleanup(FCLState* state) {
	state->idMap.clear();
	state->objects.clear();
	delete state->manager;
	delete state;
}

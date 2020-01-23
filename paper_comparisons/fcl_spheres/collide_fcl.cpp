#include "collide_fcl.h"
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

/// @brief Collision data stores the collision request and the result given by collision algorithm.
template <typename S>
struct CollisionData
{
	CollisionData(){}

	/// @brief Collision request
	CollisionRequest<S> request;

	/// @brief Collision result
	CollisionResult<S> result;
};

template <typename S>
bool collisionCountFunction(CollisionObject<S>* o1, CollisionObject<S>* o2, void* cdata_) {
	auto* cdata = static_cast<CollisionData<S>*>(cdata_);
	const auto& request = cdata->request;
	auto& result = cdata->result;

	collide(o1, o2, request, result);

	return false;
}


struct FCLState {
	BroadPhaseCollisionManagerf* manager = nullptr;
	std::unordered_map<CollisionObject<Float>*,int>  idMap;
	std::vector<CollisionObject<Float>*> objs; 
};


void initializeDataForFCL(const std::vector<Vector3<Float>>& centers, BroadPhaseCollisionManagerf* manager, std::vector<CollisionObject<Float>*>& objs, std::unordered_map<CollisionObject<Float>*,int> & idMap) {
	objs.resize(centers.size());
	Transform3f pose = Transform3f::Identity();
	// set mesh triangles and vertice indices
	for (int i = 0; i < (int)centers.size(); ++i) {
		Transform3<S> pose;
  		pose.translation() = centers[i];
		CollisionObjectf* obj = new CollisionObjectf(std::make_shared<Sphere<S>>(RADIUS), pose);
		obj->computeAABB();
		manager->registerObject(obj);
		idMap[obj] = i;
		objs[i] = obj;
	}
}

FCLState* initializeFCL(const std::vector<Vector3<Float>>& centers, int managerIndex) {
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
  	case 6:
  		{
  			Vector3<S> lower_limit, upper_limit;
  			SpatialHashingCollisionManager<S>::computeBound(state->objs, lower_limit, upper_limit);
  			S cell_size = std::min(std::min((upper_limit[0] - lower_limit[0]) / 20, (upper_limit[1] - lower_limit[1]) / 20), (upper_limit[2] - lower_limit[2])/20);
  			state->manager = new SpatialHashingCollisionManager<S, detail::SparseHashTable<AABB<S>, CollisionObject<S>*, detail::SpatialHash<S>> >(cell_size, lower_limit, upper_limit);
  		}
  		break;
  	case 7:
  		{
  			Vector3<S> lower_limit, upper_limit;
  			SpatialHashingCollisionManager<S>::computeBound(state->objs, lower_limit, upper_limit);
  			S cell_size = 4.0f * RADIUS * 1.1f;//std::min(std::min((upper_limit[0] - lower_limit[0]) / 20, (upper_limit[1] - lower_limit[1]) / 20), (upper_limit[2] - lower_limit[2])/20);
  			state->manager = new SpatialHashingCollisionManager<S, detail::SparseHashTable<AABB<S>, CollisionObject<S>*, detail::SpatialHash<S>> >(cell_size, lower_limit, upper_limit);
  		}
  		break;
	default:
	  printf("ERROR, INVALID COLLISION MANAGER TYPE");
	  exit(0);
	}

	initializeDataForFCL(centers, state->manager, state->objs, state->idMap);
	return state;
}

double updateFCLPositions(FCLState* state, const std::vector<Vector3<Float>>& centers) {
	double before = GetCurrentTimeInSeconds();
	for (int i = 0; i < state->objs.size(); ++i) {
		state->objs[i]->setTranslation(centers[i]);
		state->objs[i]->computeAABB();
	}
	state->manager->update();
	double updateTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Update Position Time: %g ms\n", updateTime*1000.0);
	return updateTime;
}

double buildFCLAccelerationStructure(FCLState* state) {
	double before = GetCurrentTimeInSeconds();
	state->manager->setup();
	double buildTime = GetCurrentTimeInSeconds() - before;
	printf("FCL Acceleration Build: %g ms\n", buildTime*1000.0);
	return buildTime;
}

double fclCollision(FCLState* state, int& contactCount) {
	CollisionData<Float> collision_data;
	collision_data.request.enable_contact = false;
	collision_data.request.num_max_contacts = 100000;

	// Self collision query
	double before = GetCurrentTimeInSeconds();
	state->manager->collide(&collision_data, collisionCountFunction);
	double collisionTime = GetCurrentTimeInSeconds() - before;
	contactCount = (int)collision_data.result.numContacts();
	printf("FCL Collision Time: %g ms\n", collisionTime*1000.0);
	printf("FCL Component Collision Num Contacts: %d\n", contactCount);
	return collisionTime;
}

void fclCleanup(FCLState* state) {
	state->idMap.clear();
	state->objs.clear();
	delete state->manager;
	delete state;
}

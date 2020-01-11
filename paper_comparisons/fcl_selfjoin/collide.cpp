#include "collide.h"
#include "timer.h"
#include "mesh_io.h"
#include "collide_gong.h"

#include <set>
#include <fstream>
#include <iostream>
#include <unordered_set>
#include <algorithm>

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
#if USE_DOUBLE_PRECISION
typedef tensor_double_3_           vec3;
#else
typedef tensor_float_3_           vec3;
#endif
typedef tensor_row_Verts__3_      tri3;

void MeshLoad( Store store, uint32_t n_vert, uint32_t n_tri,
    Float* pos, uint32_t* vert, uint64_t* obj_id )
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
    if(v1 < v0) { std::swap(v0,v1); }
    edges.insert(std::make_pair(v0, v1));
  }
  uint32_t n_edge   = (uint32_t)edges.size();

  // convert the set of edges into arrays
  std::vector<uint32_t> e_hd(n_edge);
  std::vector<uint32_t> e_tl(n_edge);
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
    store.Verts().pos().load(reinterpret_cast<vec3*>(pos), 3*sizeof(Float));
    store.Edges().hd().load(e_hd.data(), sizeof(uint32_t));
    store.Edges().tl().load(e_tl.data(), sizeof(uint32_t));
    store.Tris().v().load(reinterpret_cast<tri3*>(vert), 3*sizeof(uint32_t));
    store.Tris().obj_id().load(obj_id, sizeof(uint64_t));
  store.Verts().endload();
  store.Edges().endload();
  store.Tris().endload();
}

inline std::ostream&
operator<<(std::ostream &out, vec3 o) {
  out << "(" << o.d[0] << "," << o.d[1] << "," << o.d[2] << ")";
  return out;
}



void MeshIsctRead(Store store, const std::vector<int>& startIDs, std::vector<ComparisonContact>& contacts) {
    uint32_t n_isct     = store.TTcontacts().size();
    std::cout << "**Gong: Found " << n_isct << " triangle-triangle intersections." << std::endl;

    uint64_t* o0          = store.TTcontacts().obj0().readwrite_lock();
    uint64_t* o1          = store.TTcontacts().obj1().readwrite_lock();
    uint32_t* t0          = store.TTcontacts().tri0().readwrite_lock();
    uint32_t* t1          = store.TTcontacts().tri1().readwrite_lock();
    vec3*     pos         = store.TTcontacts().pos().readwrite_lock();
    Float*    depth       = store.TTcontacts().penetration_depth().readwrite_lock();
    vec3*     nrml        = store.TTcontacts().normal().readwrite_lock();

    std::vector<Vector3<Float>> vertices;
    vertices.resize(n_isct);
    for(uint32_t k=0; k<n_isct; k++) {
        uint64_t obj0 = o0[k];
        uint64_t obj1 = o1[k];
        uint32_t o0Off = (uint32_t)startIDs[obj0];
        uint32_t o1Off = (uint32_t)startIDs[obj1];
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


void runGongCollisionOnComponents(const std::vector<Vector3<Float>>& vertices, const std::vector<Triangle>& tris, const std::vector<uint64_t>& objIDs, const std::vector<int>& startIDs, std::vector<ComparisonContact>& contacts) {
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
    Float* pos = (Float*)vertices.data();
    uint32_t* vert = smallTris.data();
    uint64_t* obj_id = (uint64_t*)objIDs.data();

    MeshLoad( store, n_vert, n_tri, pos, vert, obj_id );

    double before = GetCurrentTimeInSeconds();
    store.find_tt_iscts();
    double collisionTime = GetCurrentTimeInSeconds() - before;
    printf("gong Collision Time: %g ms\n", collisionTime*1000.0);

    MeshIsctRead(store, startIDs, contacts);
  }
}

void runCollisionTestOnConnectedComponents(std::string filename) {
    std::vector<Vector3<Float>> vertices;
    std::vector<Triangle> triangles;
    readPly(filename, vertices, triangles);
    std::vector<std::vector<Vector3<Float>>> splitVerts;
    std::vector<std::vector<Triangle>> splitTris;
    std::vector<uint64_t> objIDsPerTriangle;
    std::vector<int> startIDs;
    splitIntoConnectedComponents(vertices, triangles, splitVerts, splitTris, objIDsPerTriangle, startIDs);

    { // Collide and compare 
        std::vector<ComparisonContact> fclContacts;
        std::vector<ComparisonContact> gongContacts;
        runFCLCollisionOnComponents(splitVerts,splitTris, fclContacts);
        runGongCollisionOnComponents(vertices,triangles,objIDsPerTriangle,startIDs, gongContacts);

		auto contactOrder = [](const ComparisonContact& c0, const ComparisonContact& c1) {
			return (c0.obj0 < c1.obj0) ||
				(c0.obj0 == c1.obj0) && (
				(c0.obj1 < c1.obj1) ||
					(c0.obj1 == c1.obj1) && (
					(c0.tri0 < c1.tri0) ||
						(c0.tri0 == c1.tri0) && (
						(c0.tri1 < c1.tri1) ||
							(c0.tri1 == c1.tri1) && (
							(c0.pos.x < c1.pos.x) ||
								(c0.pos.x == c1.pos.x) && (
								(c0.nrml.x < c1.nrml.x)
									)))));

		};
		
		sort(gongContacts.begin(), gongContacts.end(), contactOrder);
		sort(fclContacts.begin(),  fclContacts.end(), contactOrder);

		auto printContacts = [](std::string filename, std::vector<ComparisonContact>& contacts) {
			FILE *fp;
			fp = fopen(filename.c_str(), "w");
			for (auto& c : contacts) {
				fprintf(fp, "%d-%d vs %d-%d at (%f %f %f), %f, (%f %f %f)\n",
					(int)c.obj0, (int)c.tri0, (int)c.obj1, (int)c.tri1, c.pos.x, c.pos.y, c.pos.z, c.depth, c.nrml.x, c.nrml.y, c.nrml.z);
			}
			fclose(fp);
		};
		printContacts("gong_contacts.txt", gongContacts);
		printContacts("fcl_contacts.txt", fclContacts);
        
		auto checkForMissing = [&contactOrder](std::vector<ComparisonContact>& contacts, std::vector<ComparisonContact>& searchWithin) {
			int missingContactCount = 0;
			for (auto& c : contacts) {
				auto search = std::lower_bound(searchWithin.begin(), searchWithin.end(), c, contactOrder);
				if (search == searchWithin.end()) {
					++missingContactCount;
					printf("Missing!!: (%d/%d)->(%d/%d) at (%f %f %f), depth %f, normal (%f %f %f)\n",
						(int)c.obj0, (int)c.tri0, (int)c.obj1, (int)c.tri1, c.pos.x, c.pos.y, c.pos.z, c.depth, c.nrml.x, c.nrml.y, c.nrml.z);
				}
				else if (!((*search) == c)) {
					auto o = *search;
					++missingContactCount;
					printf("(%d/%d)->(%d/%d) at (%f %f %f), depth %f, normal (%f %f %f) vs\n(%d/%d)->(%d/%d) at (%f %f %f), depth %f, normal(%f %f %f); MISMATCH!\n",
						(int)c.obj0, (int)c.tri0, (int)c.obj1, (int)c.tri1, c.pos.x, c.pos.y, c.pos.z, c.depth, c.nrml.x, c.nrml.y, c.nrml.z,
						(int)o.obj0, (int)o.tri0, (int)o.obj1, (int)o.tri1, o.pos.x, o.pos.y, o.pos.z, o.depth, o.nrml.x, o.nrml.y, o.nrml.z);
				}
			}
			return missingContactCount;
		};

		printf("*** # Missing Contacts in Gong : %d\n", checkForMissing(fclContacts, gongContacts));
		printf("*** # Missing Contacts in FCL  : %d\n", checkForMissing(gongContacts, fclContacts));

    }
}
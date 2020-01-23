#include "mesh_io.h"
#include <fstream>
#include <unordered_set>

#define TINYPLY_IMPLEMENTATION
#include "tinyply.h"
class manual_timer
{
	std::chrono::high_resolution_clock::time_point t0;
	double timestamp{ 0.f };
public:
	void start() { t0 = std::chrono::high_resolution_clock::now(); }
	void stop() { timestamp = std::chrono::duration<float>(std::chrono::high_resolution_clock::now() - t0).count() * 1000; }
	const double & get() { return timestamp; }
};

#   define assertM(condition, message) \
    do { \
        if (! (condition)) { \
            std::cerr << "Assertion `" #condition "` failed in " << __FILE__ \
                      << " line " << __LINE__ << ": " << message << std::endl; \
            std::terminate(); \
        } \
    } while (false)


void readPly(const std::string & filepath, std::vector<Vector3<Float>>& verts, std::vector<Triangle>& tris) {
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
			if (vertices->t != tinyply::Type::FLOAT32) {
				throw std::runtime_error("verts were not floats in " + filepath);
			}
#if USE_DOUBLE_PRECISION
			verts.resize(vertices->count);
			std::vector<Vector3<float>> singlePrecisionVerts;
			singlePrecisionVerts.resize(vertices->count);
			std::memcpy(singlePrecisionVerts.data(), vertices->buffer.get(), numVerticesBytes);
			for (int i = 0; i < verts.size(); ++i) {
				verts[i] = singlePrecisionVerts[i];
			}
#else
			verts.resize(vertices->count);
			std::memcpy(verts.data(), vertices->buffer.get(), numVerticesBytes);
#endif

			std::vector<uint32_t> indices;
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

	}
	catch (const std::exception & e) {
		std::cerr << "Caught tinyply exception: " << e.what() << std::endl;
	}
}

void writePly(const std::string & filename, const std::vector<Vector3<Float>>& verts, const std::vector<Triangle>& tris) {
	std::filebuf fb_ascii;
	fb_ascii.open(filename, std::ios::out);
	std::ostream outstream_ascii(&fb_ascii);
	if (outstream_ascii.fail()) throw std::runtime_error("failed to open " + filename);

	PlyFile plyFile;

#if USE_DOUBLE_PRECISION
	plyFile.add_properties_to_element("vertex", { "x", "y", "z" },
		Type::FLOAT64, verts.size(), reinterpret_cast<uint8_t*>(const_cast<Vector3<Float>*>(verts.data())), Type::INVALID, 0);
#else
	plyFile.add_properties_to_element("vertex", { "x", "y", "z" },
		Type::FLOAT32, verts.size(), reinterpret_cast<uint8_t*>(const_cast<Vector3<Float>*>(verts.data())), Type::INVALID, 0);
#endif

	std::vector<uint32_t> indices;
	indices.resize(3 * tris.size());
	for (int t = 0; t < tris.size(); ++t) {
		for (int i = 0; i < 3; ++i) indices[3 * t + i] = (uint32_t)tris[t][i];
	}
	if (indices.size() > 0) {
		plyFile.add_properties_to_element("face", { "vertex_indices" },
			Type::UINT32, tris.size(), reinterpret_cast<uint8_t*>(indices.data()), Type::UINT8, 3);
	}

	plyFile.get_comments().push_back("generated by tinyply 2.2");

	plyFile.write(outstream_ascii, false);
}


void splitIntoComponents(const std::vector<Vector3<Float>>& verts, const std::vector<Triangle>& tris,
	const std::vector<int>& vertexStartLocations,
	std::vector<std::vector<Vector3<Float>>>& outVerts, std::vector<std::vector<Triangle>>& outTris) {
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
		memcpy(outVerts[i].data(), verts.data() + startLocation, componentVertCount * sizeof(Vector3<Float>));
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



void splitIntoConnectedComponents(const std::vector<Vector3<Float>>& verts, const std::vector<Triangle>& tris,
	std::vector<std::vector<Vector3<Float>>>& outVerts, std::vector<std::vector<Triangle>>& outTris, std::vector<ObjID>& outObjIDs, std::vector<int>& startTriIDs) {

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
		[&adjacency, &foundComponent, &dfs](int vert, std::unordered_set<int>& component) {
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
		int end = (int)verts.size();
		if (i < vertexStartIDs.size() - 1) {
			end = vertexStartIDs[i + 1];
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
	printf("startTriIDs ****\n");
	for (int i = 0; i < startTriIDs.size(); ++i) {
	    printf("%d: %d\n", i, startTriIDs[i]);
	}
	printf("************\n");
	*/

}

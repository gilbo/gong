#pragma once
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>


#define ERR(msg) { \
 \
  std::cerr << "ERROR: " << msg << std::endl; \
  exit(1); \
}

/*
	Loads a (potentially binary) OFF file of a triangle mesh into positions and indices.
	Every triplet of float in positions defines a vertex position
	Every triplet of uint32_t defines a triangle (indexing into positions)
*/
static void loadOFF(std::string filename, std::vector<float>& positions, std::vector<uint32_t>& indices) {
	if (filename.back() == 'b') { // Nonstandard binary version of OFF
		printf("Warning, loading nonstandard binary OFF file %s\n", filename.c_str());
		// binary
		FILE* file = fopen(filename.c_str(), "rb");
		char header[4] = {};
		fread(header, sizeof(header), 1, file);

		int vertexCount = 0;
		int triCount = 0;
		int edgeCount = 0;
		fread(&vertexCount, sizeof(vertexCount), 1, file);
		fread(&triCount, sizeof(triCount), 1, file);
		fread(&edgeCount, sizeof(edgeCount), 1, file);

		positions.resize(vertexCount);
		fread(positions.data(), sizeof(float) * 3 * vertexCount, 1, file);

		// note - assuming 3 vertices per polygon (triangles)
		indices.resize(triCount * 3);
		fread(indices.data(), sizeof(int) * 3 * triCount, 1, file);

		fclose(file);
	} else {
		std::ifstream OFF(filename.c_str());
		if(!OFF.good()) { ERR("could not open '" << filename << "'"); }
		uint32_t n_vert   = 0;
		uint32_t n_tri    = 0;
		uint32_t junk;
		std::string OFF_sig;
		OFF >> OFF_sig >> n_vert >> n_tri >> junk;
		if(!OFF.good()) { ERR("file-read error in header of '" << filename << "'"); }
		if(OFF_sig != "OFF") { ERR("expected OFF file marker"); }

		positions.resize(3*n_vert);
		indices.resize(3*n_tri);

		for(uint32_t k=0; k<n_vert; k++) {
			OFF >> positions[3*k+0] >> positions[3*k+1] >> positions[3*k+2];
			//printf("%f %f %f\n", positions[3*k+0], positions[3*k+1], positions[3*k+2]);
		}
		if(!OFF.good()) { ERR("file-read error in positions portion of '" << filename << "'"); }
		for(uint32_t k=0; k<n_tri; k++) {
			uint32_t count;
			OFF >> count >> indices[3*k+0] >> indices[3*k+1] >> indices[3*k+2];
			//printf("%u %u %u\n", indices[3*k+0], indices[3*k+1], indices[3*k+2]);
			if(count != 3) { ERR("expected only triangular faces"); }
		}
		if(OFF.bad()) { ERR("file-read error in indices portion of '" << filename << "'"); }

		OFF.close();
	}
}


/*
	Loads a (potentially binary) RFF file of a collection of rays into origins and directions
	Every triplet of float in positions defines a vertex position
	Every triplet of uint32_t defines a triangle (indexing into positions)
*/
static void loadRFF(std::string filename, std::vector<float>& origins, std::vector<float>& directions) {
	struct SimpleRay {float o[3]; float d[3];};

	std::vector<SimpleRay> rays;
	if (filename.back() == 'b') {
		// binary
		FILE* file = fopen(filename.c_str(), "rb");
		char header[4] = {};
		fread(header, sizeof(header), 1, file);

		int rayCount = 0;
		fread(&rayCount, sizeof(rayCount), 1, file);

		rays.resize(rayCount);
		fread(rays.data(), sizeof(SimpleRay) * rayCount, 1, file);

		fclose(file);
	} else {
		std::ifstream inFile(filename.c_str());
		if(!inFile.good()) { ERR("could not open '" << filename << "'"); }
		std::string line;
		std::getline(inFile, line);
		// TODO: check that its RFF
		std::getline(inFile, line);
		std::istringstream in(line);
		int numRays;
		in >> numRays;

		rays.resize(numRays);

		for (int i = 0; i < numRays; ++i) {
			std::string line;
			std::getline(inFile, line);
			std::istringstream in(line);
			in 	>> rays[i].o[0] >> rays[i].o[1] >> rays[i].o[2] 
				>> rays[i].d[0] >> rays[i].d[1] >> rays[i].d[2];
		}
	}

	// Convert to de-interleaved form
	origins.resize(rays.size()*3);
	directions.resize(rays.size()*3);
	for (int i = 0; i < rays.size(); ++i){
		for (int j = 0; j < 3; ++j) {
			origins[i*3+j] = rays[i].o[j];
			directions[i*3+j] = rays[i].d[j];
		}
	}

}


// TODO: saveOFF(), loadOFF()
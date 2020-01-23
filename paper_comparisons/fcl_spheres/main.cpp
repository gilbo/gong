#include "collide_fcl.h"
#define ERR(msg) { \
 \
  std::cerr << "ERROR: " << msg << std::endl; \
  exit(1); \
}

void ReadPerf( std::vector<int>& frames, std::string filename ) {
  std::ifstream F(filename);
  if(!F.good()) { ERR("could not open '" << filename << "'"); }

  int frame, ms;
  std::string tmpstr;
  while(getline(F, tmpstr, ',')) {
    frame = stoi(tmpstr);
    getline(F, tmpstr, '\n');
    ms = stoi(tmpstr);                                                                                                                                                   
    frames.push_back(frame);
  }
                                                                                                                              
  F.close();
}



void LoadSpheres( std::vector<Vector3<Float>>& centers, std::string filename ) {
  Float x,y,z;

  std::ifstream YF(filename);
  if(!YF.good()) { ERR("could not open '" << filename << "'"); }

  int n_yarns, n_samp;

  YF >> n_yarns;
  //printf("Number of yarns: %d\n", n_yarns);
  for(int i=0; i<n_yarns; i++) {
    YF >> n_samp;
    for(int k=0; k<n_samp; k++) {
      YF >> x >> y >> z;
      centers.push_back({x,y,z});
    }
  }
  //printf("Number of spheres: %d\n", (int)centers.size());
  if(!YF.good()) { ERR("file-read error in '" << filename << "'"); }
  YF.close();
}


class CollisionSequence {
	std::vector<std::vector<Vector3<Float>>> m_centers;
	int m_fclManagerIndex;
	FCLState*	m_fclState;
	std::ofstream m_outputStream;
public:
	CollisionSequence() {}
	CollisionSequence(const std::vector<std::string>& filenames, std::string perfFile, int fclManagerIndex);
	void solveAll(bool rebuild = false);
private:

};

CollisionSequence::CollisionSequence(const std::vector<std::string>& filenames, std::string perfFile, int fclManagerIndex) {
	m_centers.resize(filenames.size());
	for (int i = 0; i < filenames.size(); ++i) {
		LoadSpheres(m_centers[i], filenames[i]);
	}
	std::cout << "Load Complete!" << std::endl;
    m_fclManagerIndex = fclManagerIndex;
	m_outputStream.open(perfFile);
	m_outputStream << "Frame,FCL Total,FCL Init,FCL Build,FCL Rebuild,FCL Collision,FCL Contact Count" << std::endl;
	
}


void CollisionSequence::solveAll(bool doRebuild) {
	std::cout << "Frame: " << 0 << std::endl;
	int contactCount = 0;
	int totalContacts = 0;
	auto reinitializeAndSolve = [this, &contactCount,&totalContacts](int frameIndex) {
	  	std::cout << "reinit and solve" << std::endl;
	    double before = GetCurrentTimeInSeconds();
		m_fclState = initializeFCL(m_centers[frameIndex], m_fclManagerIndex);

		double after = GetCurrentTimeInSeconds();
		double fclInitTime = (after - before);
		double fclBuildTime = 0.0;
		double fclCollisionTime = 0.0;

		fclBuildTime = buildFCLAccelerationStructure(m_fclState);
		fclCollisionTime = fclCollision(m_fclState, contactCount);
		totalContacts += contactCount;
		double fclTotal = fclInitTime + fclBuildTime + fclCollisionTime;

		m_outputStream << frameIndex << "," << fclTotal << "," << fclInitTime << "," << fclBuildTime << ",," << fclCollisionTime << "," << contactCount << std::endl;
	};
	reinitializeAndSolve(0);
	
	for (int i = 1; i < m_centers.size(); ++i) {
		double fclTotal = 0.0;
		std::cout << "Frame: " << i << std::endl;
		if (doRebuild || ((i % 8) == 0)) {
			fclCleanup(m_fclState);
			reinitializeAndSolve(i);
		} else {
			double fclUpdateTime = 0.0;
			double fclCollisionTime = 0.0;
			fclUpdateTime = updateFCLPositions(m_fclState, m_centers[i]);
			fclCollisionTime = fclCollision(m_fclState, contactCount);
			totalContacts += contactCount;

			double fclTotal = fclUpdateTime + fclCollisionTime;
			m_outputStream << i << "," << fclTotal << ",,," << fclUpdateTime << "," << fclCollisionTime << "," << contactCount << std::endl;
		}
	}
	fclCleanup(m_fclState);
	m_outputStream.close();
	printf("Average contact count: %f\n",(double)totalContacts/(double)m_centers.size());
}



void benchmark(std::string outputFile, std::string pathPrefix, std::string pathSuffix,  bool rebuildEveryFrame, int fclManagerIndex) {
	std::vector<std::string> filenames;
	std::vector<int> frames;
	ReadPerf(frames,pathPrefix+"contactperf.txt");
	for (int i : frames) {
		char filename[100];
		printf("%sframe%d%s\n", pathPrefix.c_str(),i,pathSuffix.c_str());
		sprintf(filename, "%sframe%d%s", pathPrefix.c_str(),i,pathSuffix.c_str());
		filenames.push_back(filename);
	}
	std::cout << "Benchmark" << std::endl;
	CollisionSequence sequence(filenames, outputFile, fclManagerIndex);
	sequence.solveAll(rebuildEveryFrame);
}

int main() {
	const std::vector<int> validManagerIndices = {0,1,2,3,4,5,6,7};
	const std::vector<std::string> manager_names = {"Naive","SSaP", "SaP","IntervalTree","DynamicAABBTree","DynamicAABBTree","SpatialHash","GongSizedSpatialHash"};
	const int endIndex = 24;

	std::vector<std::string> benchmarks = {"alt-diag","seersucker","stock-garter"};
	for (auto b : benchmarks) { 
		std::string pathPrefix = "../data/yarnsim/" + b + "/";
		for (int i : validManagerIndices) {
			benchmark(b+"_rebuild_"+manager_names[i]+".csv", pathPrefix, ".txt",  true, i);
			benchmark(b+"_refit_"+manager_names[i]+".csv", pathPrefix, ".txt",  false, i);
		}
		
	}

}



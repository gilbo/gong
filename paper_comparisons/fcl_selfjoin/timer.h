#pragma once
#ifdef _WIN32
#include <Windows.h>
static double GetCurrentTimeInSeconds() {
    unsigned __int64 pf;
    QueryPerformanceFrequency((LARGE_INTEGER *)&pf);
    double freq_ = 1.0 / (double)pf;

    unsigned __int64 val;
    QueryPerformanceCounter((LARGE_INTEGER *)&val);
    return (val)* freq_;
}
#else
#include <sys/time.h>
static double GetCurrentTimeInSeconds() {
    struct timeval timevalue;
    gettimeofday(&timevalue, nullptr);
    return (double)((unsigned long long)timevalue.tv_sec) + (double)((unsigned long long)timevalue.tv_usec) / 1000000.0;
}
#endif

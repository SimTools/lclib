#include <unistd.h>
int getcwd_(char * cwd, int *lcwd){
	int    ir;
	ir = (int)getcwd(cwd,(size_t)*lcwd);
	return (ir);
}

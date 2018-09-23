# Ascension
A metaheuristic optimization framework. See the [project homepage](http://inversed.ru/Ascension.htm) for an overview and notable results (but note that some of the described features had to be omitted from the 2.0 release). 

## How to use the framework
Copy a project definition module (PDM) into the project folder, rename it to *problem.pas*, then compile and run Ascension. A couple of example PDMs are located in the *problems* folder. Algorithm parameters are specified in the *Config.ini* file.
Several files are created during the optimization process:

File | Description
--- | ---
XX_Status | Status file containing the data about the optimization process (XX is the abbreviated algorithm name)
XX_Best | The best solution found during the optimization.
Runs | Information about the results of multiple optimization runs
Runs_Best | The best solution found during multiple optimization runs

## How to create a PDM
You can specify your own problem by creating a *problem.pas* file that supplies the types and routines required by each metaheuristic (a complete list can be found in *interface.inc*). See *Problem_NQ.pas* from the *problems* folder for an example of a complete PDM. If you don't need a certain algorithm, the corresponding definitions can be effectively omitted by placing a *{$I DummyXX.inc}* line at the end of the file.

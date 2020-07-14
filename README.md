# VASP Guideline

Basic notes on how to use **VASP** 

## **General comments regarding VASP**:

In order to run **VASP**, you need 4 input files: **INCAR** (contains calculation parameters), **POSCAR** (contains the geometry of the system), **POTCAR** (contains psuedopotential information), **KPOINTS** (contains which kpoint you want to use).

## **INCAR**:

1. **ISTART** = Initial guess of the orbitals

* | 0=new, 1=continue from WAVECAR with const E cut-off, 2=continue from WAVECAR with const basis |

2. **ICHARG** = Initial guess of charge density           

* | 0=from orbitals, 1=read from CHGCAR, 2=from atomic charges, +10=non-scf(const), 11=to get evalue for band plots or DOS read from CHGCAR |

3. **LCHARG** = Whether to save CHGCAR or not       

* | .FALSE. or .TRUE. |

4. **SYSTEM** = Name of the system ( for example pt4sn3-sio2)

5. **ENCUT**  = Plane Wave cut-off Energy in eV 

6. **LREAL**  = Real space projection 

* | .FALSE. or .AUTO. |

* **Note**: For large supercells or hybrid functionals 'Auto' is recommended (faster with a negligible loss in accuracy).

7. **ISMEAR** = Smearing method             

* | 0=gaussian, -1=fermi, -4=tetrahedron, -5=blochl |

8. **SIGMA**  = Broadening of Smearing in eV (width of smearing)

* | recommended: 0.1 |

* **Note**: For calculating DOS and very accurate total energy calculations (no relaxation in metals) use the tetrahedron method (ISMEAR=-5).

9. **NELM**   = Num of electronic relaxation steps (SCF)

10. **NELMIN** = Min electronic relaxation steps that should be done between each ionic (geometric) update.

11. **EDIFF**  = Stopping criteria for ELM (electronic minimization, SCF)

* | recommended: 1e-6 |

12. **IBRION** = Relaxation method            

* | 0=MD run, 1=quasi new(RMM-DIIS for geom opt)RECOMMENDED FOR NEB, 2=CG algorithm(for hard relaxation), -1=static, 5-8=freq; 7,8 using DFTPT; 5,6 using finite displacement, 6,8 with sym; 5,7 w/o sym |  

* **Note**: For difficult relaxation problems it is recommended to use the conjugate gradient algorithm (IBRION=2).

* **Note**: Damped molecular dynamics (IBRION=3) are often useful when starting from very bad initial guesses.

* **Note**: Close to the local minimum the RMM-DIIS (IBRION=1) is usually the best choice.

13. **EDIFFG** = Stopping criteria for ionic (geometric) relaxation, IOM (geometry optimizations)

* | Default : EDIFFx10, recommended: -0.01 |

* **Note**: If EDIFFG is negative it has a different meaning: In this case the relaxation will stop if all forces are small than the  |EDIFFG|. This is usually a more convenient setting. EDIFFG does not apply for MD-simulations.

14. **NSW** = Num of steps for ionic relaxtion(geometry).

15. **ALGO**   = Algorithm used for SCF (electronic minimization)         

* | Normal=Davidson, Fast=RMM, Very Fast=RMM-DIIS, All=good for hybrid functionals |

16. **ISPIN**  = Whether the calculation is spin polarized or not             

* | 2=spin polarized, 1=no |

17. **NUPDOWN** = Num of e with spin up - spin down = -1(default,full relaxation)

* | 0-singlet, 1-doublet, etc  |

18. **ISIF**   = Used for relaxing the unit cell           

* | 2=relax ions only, 3=ions + volume |

19. **LORBIT** = Density of States calculations            

* | use 11 for projected DOS, LORBIT<10, the spd- and site projected wavefunction character of each band is, evaluated, and the local partial DOS is calculated. LORBIT>=10, RWIG is ignored, works only for PAW method |


20. **PREC**   = Precision of the calculation        

* | Accurate or Normal |

21. **LWAVE**  = Save Wavecar or not       

* | .FALSE. or .TRUE. |

22. **POTIM**  = Trial step, time-step for ion-motion in MD calculations in fs

23. **IVDW**   = VDW correction 

* | recommended: 11 |

&nbsp; &nbsp;

### **Functionals**:

24. **GGA** = Override the type of density functional specified in the POTCAR

* | default: PBE = PE |

25. **LHFCALC** = Switch on Hybrid and Hartree-Fock type calculations

* | .FALSE. or .TRUE. |

&nbsp; &nbsp;

* **HF** Setting:
* LHFCALC = .TRUE.       
* AEXX = 1.0   
* ALDAC = 0.0  
* AGGAC = 0    

&nbsp; &nbsp;

* **B3LYP** Functional Setting:
* LHFCALC = .TRUE.       
* GGA = B3               
* AEXX = 0.2             
* AGGAX = 0.72           
* AGGAC = 0.81
* ALDAC = 0.19

&nbsp; &nbsp;

* **PBE0** Functional Setting:
* GGA = PE          
* LHFCALC = .TRUE.  

&nbsp; &nbsp;

* **HSE06** Functional Setting:
* LHFCALC = .TRUE.  
* HFSCREEN = 0.2  

## **POSCAR**:

Pt2Si cluster isolated from MgO &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; ! comment line

   1.00000000000000 &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; ! universal scaling factor
   
   12.6256980228000000  &nbsp; &nbsp; &nbsp;  0.0000000000000000  &nbsp; &nbsp; &nbsp;  0.0000000000000000 &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; ! first  Bravais lattice vector
   
   0.0000000000000000 &nbsp; &nbsp; &nbsp;  12.6256980228000000 &nbsp; &nbsp; &nbsp;   0.0000000000000000 &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;  ! second Bravais lattice vector
   
   0.0000000000000000  &nbsp; &nbsp; &nbsp;  0.0000000000000000 &nbsp; &nbsp; &nbsp;  30.0000000000000000  &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; ! third  Bravais lattice vector
     
   Si  &nbsp; &nbsp; &nbsp;  Pt &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;    ! elements
   
   1 &nbsp; &nbsp; &nbsp;   2 &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; ! num of each element
     
Selective dynamics &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; ! for relaxing some coordinates, F=fixed, T=relax

Cartesian &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; ! direct (fractional) or cartesian (only first letter is significant)

 4.210900000000  &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;  4.218200000000 &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;  12.610000000000  &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;     F &nbsp; &nbsp; &nbsp;    F  &nbsp; &nbsp; &nbsp;  T
 
 6.205100000000 &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;   4.215700000000 &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;  13.363500000000  &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;     T &nbsp; &nbsp; &nbsp;    T  &nbsp; &nbsp; &nbsp;  F
 
 2.216900000000  &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;  4.239300000000 &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;  13.364400000000   &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;    F &nbsp; &nbsp; &nbsp;    F  &nbsp; &nbsp; &nbsp;  F

* The positions can be given in direct (fractional) or Cartesian coordinates. Note that the lattice vectors are always scaled by the universal scaling factor.

## **POTCAR**:

* It contains information regarding the psuedopotential of all atoms in the system which can be found in **VASP** psuedopotential directory. Make sure that the order of atoms in **POTCAR** is the same as **POSCAR**.

## **KPOINTS**:

K-Points       &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;      ! comment

0              &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;      ! 0 = automatic generation of k-points

Monkhorst Park &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;      ! M use Monkhorst Pack OR Gamma (gamma centered grid)

1  1  1   &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;           ! grid 1x1x1 (subdivision along each rec lattice vec.)

0  0  0   &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;           ! shift (usually 0 0 0) = origin of the k-mesh, M-P without shift = Gamma with a 0.5 0.5 
0.5 shift

* For very large mesh it takes a lot of cpu-time to generate the mesh. Therefore if you want to use the same k-mesh very frequently do the automatic generation only once and copy the file IBZKPT to the file KPOINTS.

### For band structure calculations:

K-Points     &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        ! comment

40           &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        ! 40 intersections: 10 points between each gamma/X/W

Line-mode    &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        

cart         &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        ! cartesian or reciprocal

 0   0   0   &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        ! gamma
 
 0   0   1   &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        ! X
 
 0   0   1   &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        ! X
 
 0.5 0   1   &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        ! W

 0.5 0   1   &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        ! W
 
 0   0   1   &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;        ! gamma

* This particular mode is useful for the calculation of band-structures. When band structures are calculated, it is required to perform a fully self-consistent calculation with a full k-point grid first, and to perform a non-selfconsistent calculation next (with using e.g. ICHARG=11; **see Band Structure Calculations**).

## **Bader Charge Calculations**:

* LAECHG = .TRUE.        

* For Bader charge analysis the core charge will be written so that it would be used in Bader. In quantum espresso it is recommended to perform the bader analysis only if PAW pseudopotentials are employed. Elisa made some test using QE and ultra-soft pseudopotentials (what we usually use) and VASP with PAW pseudop. The results are very similar. However, for VASP there is a correction to consider also the core electrons. In this case Elisa obtained different results (and they make more sense), specially with atoms such as B or Al. 

* So the best way to proceed: To use this correction you need to include in your INCAR file: LAECHG=.TRUE.

* Once the calculation is done to extract the charges you need vtst-scripts in **VASPHOME**:
* ./chgsum.pl AECCAR0 AECCAR2
* ./bader -vac off CHGCAR -ref CHGCAR_sum

## **AIMD Calculations**:

* For MD, we recommend the use of PREC=Normal although PREC=Low yields often satisfactory results.                                  

* MAXMIX = 40, Reuse mixer from one MD step to next

* NELMIN = 4, Minimum 4 steps per time step, avoid breaking after 2 steps

* IBRION = 0

* LREAL = A

* NSW = 10000

* NWRITE = 0

* LCHARG = .FALSE.

* LWAVE = .FALSE.

* TEBEG =   2000, Temperature begin

* TEEND =  2000, Temperature end

* SMASS = 0, -1,  0 = NVT-canonical (Nose) MD, -1 = NVE-micro canonical MD with temperature scaling every 50 steps

* NBLOCK = 50

* POTIM = 1.0, timestep in fs

* POTIM = 1.0

* IWAVPR = 12, Determines how orbitals and/or charge densities are extrapolated from one ionic configuration to the next configuration. MD=12, relax=11

* **Note**: Use ALGO=Very Fast (RMM-DIIS for electrons) for large molecular dynamics runs.For surface or difficult systems, you might need to increase NELMIN = 8.

## **NEB Calculations**:

* IBRION = 1, recommended

* ALGO   = fast

* ICHAIN = 0, (int) Indicates which method to run. NEB (ICHAIN=0) is the default.

* IMAGES = 5, (int) Number of NEB images between the fixed endpoints.

* SPRING = -5.0, (float) The spring constant, in eV/Ang^2 between the images; negative value turns on nudging

* LCLIMB = .TRUE., Flag to turn on the climbing image algorithm (CI-NEB)

* LTANGENTOLD = .FALSE., Flag to turn on the old central difference tangent

* LDNEB = .FALSE., Flag to turn on modified double nudging

* LNEBCELL = .FALSE., Flag to turn on SS-NEB. Used with ISIF=3 and IOPT=3.


## **Band Structure Calculations**:

## **Work Function Calculations**:

* LVTOT  = .TRUE., to generate a LOCPOT-file

* IDIPOL = 3, dipole correction in all x,y,z

* LDIPOL = .TRUE.

* LVHAR  = .TRUE., So that the LOCPOT contains only the electrostatic potential and not the entire local potential.

## **Solvation Calculations**:

* LSOL = .TRUE.

* LAMBDA_D_K = 3.0, Debye screening length

* EB_K = 78.4, Relative permittivity of water

* TAU = 0, Effective cavity surface tension


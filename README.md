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

**Functionals**:

&nbsp; &nbsp;

24. **GGA** = Override the type of density functional specified in the POTCAR

* | default: PBE = PE |

25. **LHFCALC** = Switch on Hybrid and Hartree-Fock type calculations

* | .FALSE. or .TRUE. |

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

## **POTCAR**:

## **KPOINTS**:



## **Band Structure Calculations**:



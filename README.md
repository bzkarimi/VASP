# VASP Guideline

Basic notes on how to use **VASP** 

## **General comments regarding VASP**:

In order to run **VASP**, you need 4 input files: **INCAR** (contains calculation parameters), **POSCAR** (contains the geometry of the system), **POTCAR** (contains psuedopotential information), **KPOINTS** (contains which kpoint you want to use).

## **INCAR**:

1. **ISTART** = Initial guess of the orbitals

| 0=new, 1=continue from WAVECAR with const E cut-off, 2=continue from WAVECAR with const basis |

2. **ICHARG** = Initial guess of charge density           

| 0=from orbitals, 1=read from CHGCAR, 2=from atomic charges, +10=non-scf(const), 11=to get evalue for band plots or DOS read from CHGCAR |

3. **LCHARG** = Whether to save CHGCAR or not       

| .FALSE. or .TRUE. |

4. **SYSTEM** = Name of the system ( for example pt4sn3-sio2)

5. **ENCUT**  = Plane Wave cut-off Eenergy in eV 

6. **LREAL**  = Real space projection 

| .FALSE. or .AUTO. |

**Note:**: For large supercells or hybrid functionals 'Auto' is recommended (faster with a negligible loss in accuracy)

ISMEAR = 0             | 0=gaussian, -1=fermi, -4=tetrahedron, -5=blochl |

SIGMA  = 0.1           | broadening in eV (width of smearing)
                       | For the calculations of the DOS and very accurate total energy calculations
                        (no relaxation in metals) use the tetrahedron method (ISMEAR=-5) |

NELM   = 100           | num of electronic relaxation steps |

NELMIN = 5             | min electronic relaxation steps between each ionic update |

NELMDL = -5            | no update of charge for 3 steps, 0=immediately update |

## **POSCAR**:

## **POTCAR**:

## **KPOINTS**:



## **Band Structure Calculations**:



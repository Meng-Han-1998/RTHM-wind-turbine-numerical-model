# RTHM-wind-turbine-numerical-model
# OpenFAST for Real-Time Hybrid Model Testing of Floating Wind Turbines

This repository provides a modified version of **OpenFAST** for the **numerical simulation of floating wind turbines** in **real-time hybrid model (RTHM) testing** applications.

The main objective of this project is to extend OpenFAST so that it can be compiled into a **dynamic link library (DLL)** and called by external programs such as **Python**, **MATLAB**, and **LabVIEW**. This enables tight coupling between high-fidelity wind turbine simulation and external real-time control, testing, monitoring, or hardware-in-the-loop platforms.

In addition, this repository includes a detailed example showing how to call the generated DLL from **Python**, as well as a simulation case based on the **IEA 15 MW reference wind turbine**.

---

## Overview

**OpenFAST** is a widely used open-source tool for aero-hydro-servo-elastic simulation of wind turbines.  
However, its standard workflow is primarily designed for standalone execution.

For real-time hybrid model testing of floating wind turbines, it is often necessary to:

- embed the wind turbine numerical model into an external execution framework,
- exchange data with third-party software in real time,
- drive simulations step-by-step,
- and integrate with control, sensing, and actuator subsystems.

To support these needs, this repository provides an OpenFAST-based solution that:

- modifies OpenFAST for external-program interaction,
- exposes callable interfaces through a DLL,
- supports integration with platforms such as **MATLAB**, **Python**, and **LabVIEW**,
- and demonstrates a practical floating offshore wind turbine simulation workflow.

---

## Key Features

- **Modified OpenFAST source code** for external invocation
- **DLL generation** for cross-program coupling
- Interfaces suitable for use in:
  - Python
  - MATLAB
  - LabVIEW
  - other DLL-compatible environments
- Designed for **real-time hybrid model testing**
- Focused on **floating offshore wind turbine** simulation
- Includes a **Python calling example**
- Includes an **IEA 15 MW simulation case**

---

## Application Background

Real-time hybrid model testing combines:

- **numerical simulation** for some subsystems, and
- **physical testing** for others,

within a unified real-time framework.

For floating wind turbines, this approach is especially useful because it allows researchers to:

- simulate aerodynamic, structural, and control dynamics numerically,
- physically test selected components or subsystems,
- and study coupled system behavior under realistic offshore conditions.

In this context, OpenFAST serves as the high-fidelity numerical core, while the DLL interface enables communication with external testing and control environments.

---

## Repository Purpose

This repository is intended for researchers and engineers who want to:

- use OpenFAST in a **callable library form** rather than as a standalone executable,
- integrate floating wind turbine simulation into external software workflows,
- perform **real-time**, **co-simulation**, or **hardware-in-the-loop** studies,
- and reproduce or extend an **IEA 15 MW floating wind turbine** simulation case.

---

## Main Contents

The repository typically contains the following components:

- **Modified OpenFAST source code**  
  Adapted to support DLL compilation and external function calls.

- **Build configuration / project files**  
  Used to compile the modified OpenFAST into a shared library / DLL.

- **Python example**  
  Demonstrates how to load the DLL, initialize the simulation, exchange data, and run the model from Python.

- **IEA 15 MW simulation case**  
  A reference case for validating and demonstrating the DLL-based workflow.

> Please adapt the directory names below to match the actual structure of your repository.

```text
.
├── src/                    # Modified OpenFAST source code
├── include/                # Header files for exported interfaces
├── build/                  # Build output directory
├── dll/                    # Generated DLL and related files
├── examples/
│   └── python/             # Python calling example
├── cases/
│   └── IEA-15MW/           # IEA 15 MW simulation case
├── docs/                   # Additional documentation
└── README.md

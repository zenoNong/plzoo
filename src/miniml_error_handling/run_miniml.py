#!/usr/bin/env python3
"""
MiniML Command Line Interface
A simple Python script to run the MiniML interpreter with exception support.
"""

import subprocess
import sys
import os

def run_miniml_file(filename):
    """Run a MiniML file using the compiled OCaml interpreter."""
    try:
        # Build the project first
        subprocess.run(["dune", "build"], check=True, cwd=".")
        
        # Run the interpreter with the file
        result = subprocess.run(["./_build/default/miniml.exe", filename], 
                              capture_output=True, text=True, cwd=".")
        
        if result.stdout:
            print(result.stdout)
        if result.stderr:
            print(result.stderr, file=sys.stderr)
            
        return result.returncode == 0
        
    except subprocess.CalledProcessError as e:
        print(f"Error building or running MiniML: {e}", file=sys.stderr)
        return False
    except FileNotFoundError:
        print("Error: dune or miniml.exe not found. Make sure you're in the miniml directory.", file=sys.stderr)
        return False

def run_miniml_interactive():
    """Run MiniML in interactive mode."""
    try:
        # Build the project first
        subprocess.run(["dune", "build"], check=True, cwd=".")
        
        # Run the interpreter interactively
        subprocess.run(["./_build/default/miniml.exe"], cwd=".")
        
    except subprocess.CalledProcessError as e:
        print(f"Error building or running MiniML: {e}", file=sys.stderr)
    except FileNotFoundError:
        print("Error: dune or miniml.exe not found. Make sure you're in the miniml directory.", file=sys.stderr)

def main():
    if len(sys.argv) > 1:
        # Run a specific file
        filename = sys.argv[1]
        if not os.path.exists(filename):
            print(f"Error: File '{filename}' not found.", file=sys.stderr)
            sys.exit(1)
        success = run_miniml_file(filename)
        sys.exit(0 if success else 1)
    else:
        # Run interactively
        run_miniml_interactive()

if __name__ == "__main__":
    main() 
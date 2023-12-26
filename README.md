# Functional Programming Labs
By Luis Daniel Casais Mezquida  
Functional Programming 22/23  
Bachelor's Degree in Computer Science and Engineering  
Universidad Carlos III de Madrid


## List of labs
- [`lab1.hs`](lab1.hs): Introduction to Haskell
- [`lab2.hs`](lab2.hs): Guards and patterns
- [`lab3.hs`](lab3.hs): Lists
- [`lab4.hs`](lab4.hs): List comprehension and Type Classes
- [`lab5.hs`](lab5.hs): Data types
- [`lab6.hs`](lab6.hs): Recursion and High Order Functions
- [`lab7.hs`](lab7.hs): Lists and High Order Functions

Guided labs can be found in [`guided/`](guided/).

## Installation
1. Install Glasgow Haskell Compiler (GHC)
2. (Optional, but recommended) Install [GHCup](https://www.haskell.org/ghcup/)

### VSCode
Install the following extensions:
- [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
- [Jupyter](https://marketplace.visualstudio.com/items?itemName=ms-toolsai.jupyter)

### VSCode + IHaskell (Jupyter Notebook)
You can also run the Jupyter Notebooks from Visual Studio Code. First you need to install both extensions in the section above. After that, you can install the IHaskell kernel.  
On Windows, you must do this with WSL. The steps in this guide are written for Ubuntu 20.04 specifically.

1. Install the needed packages for IHaskell:
```bash
sudo apt-get install -y python3-pip git libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libmagic-dev libblas-dev liblapack-dev
```

Also, in case you don't have them yet, install the required packages for the Haskell installation:
```bash
sudo apt-get install build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
```

2. Install Haskell (this will take a while)
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
The installation program will ask you for some inputs. When in doubt, just keep pressing `ENTER` for the defaults.

3. Clone the repository
```bash
git clone https://github.com/gibiansky/IHaskell
```

4. Move to the directory where you cloned the repo and install the requirements
```bash
cd IHaskell
pip3 install -r requirements.txt
```

5. Install the package with Stack (this will take a while)
```bash
sudo apt install haskell-stack
sudo stack upgrade
stack install --fast
```
This step will install GHC.

6. Now install IHaskell
```bash
ihaskell install --stack
```

Now you can open `*.ipynb` files on Visual Studio Code and run them there without depending on the university servers.  
To start the kernel, start a terminal (`Ctrl` + `J`) and run this command:
```bash
jupyter notebook --no-browser
```

You should be able to select IHaskell as the kernel and run the code cells in your notebook.  
Note: IO may not work properly.

## Execution
Just run the desired `.hs` file.

E.g.:
``` bash
runhaskell lab1.hs
```

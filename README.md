# Σλang
<p>Σλang is a simple and yet powerful core language that can be used as an interpreter for other languages</p>
<h2>Done:</h2>
<ul>
    <li>Basic expression compiler</li>
    <li>Parser</li>
    <li>Laziness</li>
</ul>
<h2>TODO:</h2>
<ul>
    <li>case & let expressions</li>
    <li>more primitives</li>
    <li>data structures (list, etc.)</li>
</ul>
<h2>Build</h2>
<strong>Tested on Ubuntu</strong></br>
You need `git`, `ghc` and `stack` packages to build Σλang</br>
Clone project
```
git clone github.com/algoritmist/Slang.git
cd Slang
stack build
```
Now you can run it:
```
stack exec -i <source_file.sl>
```
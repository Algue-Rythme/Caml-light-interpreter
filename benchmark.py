import random
import os
import time
import datetime
import matplotlib.pyplot as plt

#generate a formula
def genererFormule(nbLit, longueur):
    var = [str(x+1) for x in range(nbLit)]
    var += [str(-(x+1)) for x in range(nbLit)]
    links = [' /\\ ', ' \\/ ', ' X ', ' => ', ' <=> ']
    form = ""
    form += random.choice(var) 
    for i in range(longueur):
        form += random.choice(links)
        form += random.choice(var)
    return form + " 0"

def mesurerTemps(formule):
    init = datetime.datetime.now()
    os.system('echo "' + formule + '" | ./f2bdd > /dev/null' )
    return (datetime.datetime.now()-init).total_seconds()

def moyenne(nbLit):
    somme = 0.
    for _ in range(100):
        form = genererFormule(nbLit, nbLit*4)
        somme += mesurerTemps(form)
    return somme/50

l = []
for i in range(2, 20):
    l.append(moyenne(i))

plt.plot(list(range(2, 20)), l)
plt.show()

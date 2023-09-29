@echo off
cd assets
C:\Users\death\Documents\C65\JettMonsters\tools\tilemizer.py -b4 -m=3000 -o=logo -t="Mega65_Logo.png"
cd ..
java -jar C:\Users\death\Documents\C65\kickc\jar\kickassembler-5.19-65ce02.a.jar -bytedumpfile startup.bytes -showmem -odir C:\Users\death\Documents\C65\BAS\Mega65Startup\build -vicesymbols -log build/startup.klog src/main.asm

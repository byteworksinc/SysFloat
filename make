* echo copy -c ../orcalib/equates.asm
* copy -c ../orcalib/equates.asm

unset exit

if {#} == 0
   for i in fp math stdio2 stdlib time
      Newer obj/{i}.a {i}.asm
      if {Status} != 0
         set exit on
         echo assemble +e +t {i}.asm
         assemble +e +t {i}.asm
         unset exit
      end
   end
else
   set exit on
   for i
      assemble +e +t {i}.asm
   end
end

echo delete SysFloat
delete SysFloat

set list        fp.a math.a stdio2.a stdlib.a time.a
for i in {list}
   echo makelib SysFloat +obj/{i}
   makelib SysFloat +obj/{i}
end

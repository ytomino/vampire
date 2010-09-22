-- The Village of Vampire by YT, このソースコードはNYSLです
with Serialization;
package Tabula.Villages.Village_IO is
	
	package Village_State_IO is new Serialization.IO_Enumeration(Tabula.Villages.Village_State);
	package Village_Time_IO is new Serialization.IO_Enumeration(Tabula.Villages.Village_Time);
	
end Tabula.Villages.Village_IO;

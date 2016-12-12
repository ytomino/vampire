-- The Village of Vampire by YT, このソースコードはNYSLです
with Serialization;
package Tabula.Villages.Village_IO is
	
	package Village_State_IO is
		new Serialization.IO_Enumeration (Tabula.Villages.Village_State);
	
end Tabula.Villages.Village_IO;

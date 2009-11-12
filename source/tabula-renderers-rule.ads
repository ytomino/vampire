-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Streams;
with Web;
with System; -- with Ase.Web.Producers; avoiding compiler's bug of gcc 4.4.0
with Tabula.Villages;
with Tabula.Villages.Lists;
package Tabula.Renderers.Rule is
	
	procedure Rule_Panel(
		Object : in Tabula.Renderers.Renderer'Class;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in System.Address; -- Ase.Web.Producers.Template;
		Village_Id : in Villages.Lists.Village_Id;
		Village : in Vampire.Villages.Village_Type; 
		Player : in Boolean;
		User_Id : in String;
		User_Password : in String);
	
	procedure Change(Village : in out Vampire.Villages.Village_Type; Inputs : in Web.Query_Strings);
	
end Tabula.Renderers.Rule;

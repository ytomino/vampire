-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Tabula.Renderers.List_Page (
	Object : in Renderer'Class; 
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_List : in Villages.Lists.Village_Lists.Vector);

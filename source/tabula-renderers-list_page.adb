-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Tabula.Renderers.List_Page (
	Object : in Renderer'Class; 
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Summaries : in Villages.Lists.Summary_Maps.Map)
is
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Template : in Web.Producers.Template) is
	begin
		Handle_List (Output, Tag, Template, Object, Summaries, 9999, "", "");
	end Handle;
begin
	Produce (Object, Output, Object.Configuration.Template_List_File_Name.all, Handle'Access);
end Tabula.Renderers.List_Page;

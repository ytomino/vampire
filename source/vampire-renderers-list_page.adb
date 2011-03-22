-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.Renderers.List_Page (
	Object : in Renderer'Class; 
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map)
is
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Template : in Web.Producers.Template) is
	begin
		Handle_List (Output, Tag, Template, Object, Summaries, 9999, "", "");
	end Handle;
begin
	Produce (Object, Output, Object.Configuration.Template_Log_Index_File_Name.all, Handle'Access);
end Vampire.Renderers.List_Page;

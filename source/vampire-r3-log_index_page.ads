-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.Renderers.List_Page (
	Object : in Renderer'Class; 
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map);

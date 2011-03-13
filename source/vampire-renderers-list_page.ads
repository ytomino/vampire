-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Tabula.Renderers.List_Page (
	Object : in Renderer'Class; 
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Summaries : in Villages.Lists.Summary_Maps.Map);

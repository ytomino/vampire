-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.Renderers.Index_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Summaries : in out Tabula.Villages.Lists.Summary_Maps.Map;
	Muramura : in Natural;
	User_Id: in String;
	User_Password : in String);

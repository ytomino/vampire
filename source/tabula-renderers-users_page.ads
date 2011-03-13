-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Users.Lists;
procedure Tabula.Renderers.Users_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Summaries : in Villages.Lists.Summary_Maps.Map;
	User_List : in Users.Lists.User_Info_Maps.Map;
	User_Id : in String;
	User_Password : in String);

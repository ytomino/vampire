-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Users.Lists;
procedure Tabula.Renderers.Users_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_List : Villages.Lists.Village_Lists.Vector;
	User_List : in Users.Lists.User_Info_Maps.Map;
	User_Id : in String;
	User_Password : in String);

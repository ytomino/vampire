-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Users;
procedure Tabula.Renderers.User_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_List : in Villages.Lists.Village_Lists.Vector;
	User_Id : in String;
	User_Password : in String;
	User_Info : in Users.User_Info);
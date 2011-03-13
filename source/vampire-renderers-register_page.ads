-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Tabula.Renderers.Register_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Villages.Village_Id := Villages.Invalid_Village_Id;
	New_User_Id : in String;
	New_User_Password : in String);

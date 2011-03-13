-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.Renderers.Register_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
	New_User_Id : in String;
	New_User_Password : in String);

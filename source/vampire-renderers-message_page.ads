-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.Renderers.Message_Page(
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
	Village : access Vampire.Villages.Village_Type := null;
	Message : in String;
	User_Id : in String;
	User_Password : in String);

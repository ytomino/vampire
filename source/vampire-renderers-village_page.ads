-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Tabula.Renderers.Village_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Villages.Village_Id;
	Village : not null access constant Vampires.Villages.Village_Type;
	Day : in Natural;
	First, Last : in Integer := -1;
	Editing_Text : String := "";
	User_Id : in String;
	User_Password : in String);

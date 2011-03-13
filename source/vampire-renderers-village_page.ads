-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.Renderers.Village_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Tabula.Villages.Village_Id;
	Village : not null access constant Vampire.Villages.Village_Type;
	Day : in Natural;
	First, Last : in Integer := -1;
	Editing_Text : String := "";
	User_Id : in String;
	User_Password : in String);

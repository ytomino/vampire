-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Register_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Base_Page : in Forms.Base_Page;
	Village_Id : in Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
	New_User_Id : in String;
	New_User_Password : in String);

-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Message_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Base_Page : in Forms.Base_Page;
	Village_Id : in Tabula.Villages.Village_Id :=
		Tabula.Villages.Invalid_Village_Id;
	Village : access constant Tabula.Villages.Village_Type'Class := null;
	Message : in String;
	User_Id : in String;
	User_Password : in String);

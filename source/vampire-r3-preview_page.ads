-- The Village of Vampire by YT, このソースコードはNYSLです
with Vampire.Villages;
procedure Vampire.R3.Preview_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Image_Directory : in String;
	Village_Id : in Tabula.Villages.Village_Id;
	Village : in Villages.Village_Type;
	Message : in Villages.Message;
	User_Id : in String;
	User_Password : in String);

-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Streams;
with Tabula.Villages.Lists;
with Vampire.Forms;
with Vampire.Villages;
private with Web.Producers;
package Vampire.R3 is
	
	type Role_Images is array (Villages.Person_Role) of not null access constant String;
	
private
	
	procedure Produce (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template_Source : in String;
		Template_Cache : in String := "";
		Handler : not null access procedure (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : Web.Producers.Template));
	
	function Day_Name (
		Day : Natural;
		Today : Natural;
		State : Tabula.Villages.Village_State)
		return String;
	
	-- 村リスト
	procedure Handle_Village_List (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		Form : in Forms.Root_Form_Type'Class;
		Current_Directory : in String;
		HTML_Directory : in String;
		Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
		Log : in Boolean;
		Limits : in Natural;
		User_Id : in String;
		User_Password : in String);
	
end Vampire.R3;

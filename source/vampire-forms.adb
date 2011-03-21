-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories.Hierarchical_File_Names;
package body Vampire.Forms is
	
	procedure Write_Link_To_Resource (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		Resource : in String) is
	begin
		String'Write (Stream,
			"""" &
			Ada.Directories.Hierarchical_File_Names.Relative_Name (
				Name => Resource,
				From => Current_Directory) &
			"""");
	end Write_Link_To_Resource;
	
	function Get_New_User_Id (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Web.Element (Inputs, "id");
	end Get_New_User_Id;
	
	function Get_New_User_Password (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Web.Element (Inputs, "password");
	end Get_New_User_Password;
	
	function Get_New_User_Confirmation_Password (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Web.Element (Inputs, "password2");
	end Get_New_User_Confirmation_Password;
	
	function Is_User_List_Page (
		Form : Root_Form_Type'Class;
		Query_Strings : Web.Query_Strings)
		return Boolean is
	begin
		return Web.Element (Query_Strings, "users") = "all";
	end Is_User_List_Page;
	
	function Get_Command (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Web.Element (Inputs, "cmd");
	end Get_Command;
	
	function Get_Joining (
		Form : Root_Form_Type;
		Inputs : Web.Query_Strings)
		return Joining is
	begin
		return (
			Work_Index => Integer'Value (Web.Element (Inputs, "work")),
			Name_Index => Natural'Value (Web.Element (Inputs, "name")),
			Request => Villages.Requested_Role'Value (Web.Element (Inputs, "request")));
	end Get_Joining;
	
	function Get_Answered (
		Form : Root_Form_Type;
		Inputs : Web.Query_Strings)
		return Mark is
	begin
		declare
			X : constant Integer := Integer'Value (Web.Element (Inputs, "x"));
			Y : constant Integer := Integer'Value (Web.Element (Inputs, "y"));
			Z : constant Integer := Integer'Value (Web.Element (Inputs, "z"));
		begin
			if X + Y = Z then
				return OK;
			else
				return NG;
			end if;
		end;
	exception
		when Constraint_Error => return Missing;
	end Get_Answered;
	
	function Get_Reedit_Kind (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return Villages.Message_Kind is
	begin
		return Villages.Message_Kind'Value (Web.Element (Inputs, "kind"));
	end Get_Reedit_Kind;
	
	function Get_Action (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Web.Element (Inputs, "action");
	end Get_Action;
	
	function Get_Target (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return Villages.People.Cursor is
	begin
		return Integer'Value (Web.Element (Inputs, "target"));
	end Get_Target;
	
	function Get_Special (
		Form : Root_Form_Type'Class;
		Inputs : Web.Query_Strings)
		return Boolean is
	begin
		return Web.Checkbox_Value (Web.Element (Inputs, "special"));
	end Get_Special;
	
	procedure Set_Rule (
		Form : in Root_Form_Type'Class;
		Village : in out Villages.Village_Type;
		Inputs : in Web.Query_Strings)
	is
		procedure Process (Item : in Tabula.Villages.Root_Option_Item'Class) is
		begin
			Tabula.Villages.Change (Village, Item, Web.Element (Inputs, Item.Name));
		end Process;
	begin
		Villages.Iterate_Options (Village, Process'Access);
	end Set_Rule;
	
end Vampire.Forms;

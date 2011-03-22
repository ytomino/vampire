-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories.Hierarchical_File_Names;
package body Vampire.Forms is
	
	function Self return String is
	begin
		-- "http://.../vampire/" -> ""
		-- "http://localhost/vampire.cgi" -> "vampire.cgi"
		return Ada.Directories.Simple_Name (Web.Request_Path);
	end Self;
	
	procedure Write_Link_To_Resource (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		Resource : in String;
		Parameters : in Web.Query_Strings := Web.String_Maps.Empty_Map)
	is
		Relative : constant String :=
			Ada.Directories.Hierarchical_File_Names.Relative_Name (
				Name => Resource,
				From => Current_Directory);
	begin
		Character'Write (Stream, '"');
		String'Write (Stream, Relative);
		if Parameters.Is_Empty then
			if Relative'Length = 0 then
				String'Write (Stream, "./");
			end if;
		else
			Web.Write_Query_In_Attribute (
				Stream,
				Form.HTML_Version,
				Parameters); -- Parameters should contain ASCII only
		end if;
		Character'Write (Stream, '"');
	end Write_Link_To_Resource;
	
	procedure Write_Link_To_Index_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		User_Id : in String;
		User_Password : in String) is
	begin
		Write_Link_To_Resource (
			Stream,
			Form,
			Current_Directory => Current_Directory,
			Resource => Self,
			Parameters => Form.Parameters_To_Index_Page (User_Id, User_Password));
	end Write_Link_To_Index_Page;
	
	procedure Write_Link_To_User_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		User_Id : in String;
		User_Password : in String) is
	begin
		Write_Link_To_Resource (
			Stream,
			Form,
			Current_Directory => Current_Directory,
			Resource => Self,
			Parameters => Form.Parameters_To_User_Page (User_Id, User_Password));
	end Write_Link_To_User_Page;
	
	procedure Write_Link_To_Village_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		HTML_Directory : in String;
		Log : in Boolean;
		Village_Id : Tabula.Villages.Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		User_Id : in String;
		User_Password : in String) is
	begin
		if Log then
			Write_Link_To_Resource (
				Stream,
				Form,
				Current_Directory => Current_Directory,
				Resource => Ada.Directories.Compose (
					Containing_Directory => HTML_Directory,
					Name => Village_Id & "-" & Image (Integer'Max (0, Day)),
					Extension => "html"));
		else
			Write_Link_To_Resource (
				Stream,
				Form,
				Current_Directory => Current_Directory,
				Resource => Self,
				Parameters => Form.Parameters_To_Village_Page (
					Village_Id => Village_Id,
					Day => Day,
					First => First,
					Last => Last,
					Latest => Latest,
					User_Id => User_Id,
					User_Password => User_Password));
		end if;
	end Write_Link_To_Village_Page;
	
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
	
	function Get_Base_Page (
		Form : Root_Form_Type'Class;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return Base_Page is
	begin
		if Form.Get_Village_Id (Query_Strings) /= Tabula.Villages.Invalid_Village_Id then
			return Village_Page;
		elsif Form.Is_User_Page (Query_Strings, Cookie) then
			return User_Page;
		elsif Form.Is_User_List_Page (Query_Strings) then
			return User_List_Page;
		else
			return Index_Page;
		end if;
	end Get_Base_Page;
	
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

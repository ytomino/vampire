-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Hierarchical_File_Names;
package body Vampire.Forms is
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	function Self return String is
	begin
		-- "http://.../vampire/" -> ""
		-- "http://localhost/vampire.cgi" -> "vampire.cgi"
		return Ada.Hierarchical_File_Names.Unchecked_Simple_Name (
			Web.Request_Path);
	end Self;
	
	function Parameters_To_Base_Page (
		Form : Root_Form_Type'Class;
		Base_Page : Forms.Base_Page;
		Village_Id : Villages.Village_Id;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is
	begin
		case Base_Page is
			when Index_Page =>
				return  Form.Parameters_To_Index_Page (
					User_Id => User_Id,
					User_Password => User_Password);
			when User_Page =>
				return 	Form.Parameters_To_User_Page (
					User_Id => User_Id,
					User_Password => User_Password);
			when Forms.User_List_Page =>
				raise Program_Error with "unimplemented";
			when Forms.Village_Page =>
				return Form.Parameters_To_Village_Page (
					Village_Id => Village_Id,
					User_Id => User_Id,
					User_Password => User_Password);
		end case;
	end Parameters_To_Base_Page;
	
	procedure Write_Attribute_Name (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Name : in String) is
	begin
		String'Write (Stream, Name);
		Character'Write (Stream, '=');
	end Write_Attribute_Name;
	
	procedure Write_Attribute_Open (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class) is
	begin
		Character'Write (Stream, '"');
	end Write_Attribute_Open;
	
	procedure Write_Attribute_Close (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		renames Write_Attribute_Open;
	
	procedure Write_Link (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Root_Form_Type'Class;
		Current_Directory : in String;
		Resource : in String;
		Parameters : in Web.Query_Strings := Web.String_Maps.Empty_Map)
	is
		Relative : constant String :=
			Ada.Hierarchical_File_Names.Relative_Name (
				Name => Resource,
				From => Current_Directory);
	begin
		Write_Attribute_Open (Stream);
		if Parameters.Is_Empty then
			if Relative'Length = 0
				or else Ada.Hierarchical_File_Names.Is_Current_Directory_Name (Relative)
			then
				Web.Write_In_Attribute (Stream, Form.HTML_Version, "./");
			else
				Web.Write_In_Attribute (Stream, Form.HTML_Version, Relative);
			end if;
		else
			Web.Write_In_Attribute (Stream, Form.HTML_Version, Relative);
			Web.Write_Query_In_Attribute (
				Stream,
				Form.HTML_Version,
				Parameters); -- Parameters should contain ASCII only
		end if;
		Write_Attribute_Close (Stream);
	end Write_Link;
	
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
			Write_Link (
				Stream,
				Form,
				Current_Directory => Current_Directory,
				Resource => Ada.Hierarchical_File_Names.Compose (
					Directory => HTML_Directory,
					Relative_Name => Village_Id & "-" & Image (Integer'Max (0, Day)),
					Extension => "html"));
		else
			Write_Link (
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
	
	function Get_Group (
		Form : Root_Form_Type;
		Inputs : Web.Query_Strings)
		return Integer is
	begin
		return Integer'Value (Web.Element (Inputs, "group"));
	end Get_Group;
	
	function Get_Joining (
		Form : Root_Form_Type;
		Inputs : Web.Query_Strings)
		return Joining is
	begin
		return (
			Work_Index => Integer'Value (Web.Element (Inputs, "work")),
			Name_Index => Natural'Value (Web.Element (Inputs, "name")),
			Request => +Web.Element (Inputs, "request"));
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
		return String is
	begin
		return Web.Element (Inputs, "kind");
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
		return Villages.Person_Index'Base is
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
		Village : in out Tabula.Villages.Village_Type'Class;
		Inputs : in Web.Query_Strings)
	is
		procedure Process (Item : in Tabula.Villages.Root_Option_Item'Class) is
		begin
			Tabula.Villages.Change (Village, Item, Web.Element (Inputs, Item.Name));
		end Process;
	begin
		Tabula.Villages.Iterate_Options (Village, Process'Access);
	end Set_Rule;
	
end Vampire.Forms;

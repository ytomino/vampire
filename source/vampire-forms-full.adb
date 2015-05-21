-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Fixed;
package body Vampire.Forms.Full is
	use type Villages.Village_State;
	
	function Create return Form_Type is
	begin
		return (null record);
	end Create;
	
	overriding function HTML_Version (Form : Form_Type) return Web.HTML_Version is
	begin
		return Web.XHTML;
	end HTML_Version;
	
	overriding function Template_Set (Form : Form_Type) return Template_Set_Type is
	begin
		return For_Full;
	end Template_Set;
	
	overriding function Parameters_To_Index_Page (
		Form : Form_Type;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is
	begin
		return Web.String_Maps.Empty_Map;
	end Parameters_To_Index_Page;
	
	overriding function Parameters_To_User_Page (
		Form : Form_Type;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is
	begin
		return Parameters : Web.Query_Strings do
			Web.Include (Parameters, "user", User_Id);
		end return;
	end Parameters_To_User_Page;
	
	overriding function Parameters_To_Village_Page (
		Form : Form_Type;
		Village_Id : Villages.Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is
	begin
		return Parameters : Web.Query_Strings do
			Web.Include (Parameters, "village", Village_Id);
			if Day >= 0 then
				Web.Include (Parameters, "day", Image (Day));
			end if;
			if Day = 0 and then First = 0 then
				Web.Include (Parameters, "range", "all");
			end if;
		end return;
	end Parameters_To_Village_Page;
	
	overriding procedure Write_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Item : in String;
		Pre : in Boolean := False) is
	begin
		Web.Write_In_HTML (Stream, Web.XHTML, Item, Pre);
	end Write_In_HTML;
	
	overriding procedure Write_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Item : in String) is
	begin
		Web.Write_In_Attribute (Stream, Web.XHTML, Item);
	end Write_In_Attribute;
	
	overriding function Paging (Form : Form_Type) return Boolean is
	begin
		return False;
	end Paging;
	
	overriding function Speeches_Per_Page (Form : Form_Type) return Natural is
	begin
		return 0;
	end Speeches_Per_Page;
	
	overriding function Get_User_Id (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is
	begin
		return Web.Element (Cookie, "id");
	end Get_User_Id;
	
	overriding function Get_User_Password (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is
	begin
		return Web.Element (Cookie, "password");
	end Get_User_Password;
	
	overriding procedure Set_User (
		Form : in out Form_Type;
		Cookie : in out Web.Cookie;
		New_User_Id: in String;
		New_User_Password : in String) is
	begin
		Web.String_Maps.Include (Cookie, "id", New_User_Id);
		Web.String_Maps.Include (Cookie, "password", New_User_Password);
	end Set_User;
	
	overriding function Is_User_Page (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return Boolean
	is
		User_Id : constant String := Form.Get_User_Id (Query_Strings, Cookie);
	begin
		if User_Id'Length = 0 then
			return False;
		else
			return Web.Element (Query_Strings, "user") = User_Id;
		end if;
	end Is_User_Page;
	
	overriding function Get_Village_Id (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings)
		return Villages.Village_Id
	is
		S : constant String := Web.Element (Query_Strings, "village");
	begin
		if S'Length = Villages.Village_Id'Length then
			return S;
		else
			return Villages.Invalid_Village_Id;
		end if;
	end Get_Village_Id;
	
	overriding function Get_Day (
		Form : Form_Type;
		Village : Villages.Village_Type'Class;
		Query_Strings : Web.Query_Strings)
		return Natural
	is
		S : constant String := Web.Element (Query_Strings, "day");
	begin
		return Natural'Value (S);
	exception
		when Constraint_Error =>
			declare
				State : Villages.Village_State;
				Today : Natural;
			begin
				Village.Get_State (State, Today);
				if State /= Villages.Closed then
					return Today;
				else
					return 0;
				end if;
			end;
	end Get_Day;
	
	overriding function Get_Range (
		Form : Form_Type;
		Village : Villages.Village_Type'Class;
		Day : Natural;
		Now : Ada.Calendar.Time;
		Query_Strings : Web.Query_Strings)
		return Villages.Message_Range_Type
	is
		Message_Range : Villages.Message_Range_Type;
	begin
		if Web.Element (Query_Strings, "range") = "" then
			Message_Range := Village.Recent_Only_Message_Range (Day, Now => Now);
		else
			Message_Range := Village.Message_Range (Day);
		end if;
		return (
			First => Message_Range.First,
			Last => Integer'Max (Message_Range.First, Message_Range.Last));
	end Get_Range;
	
	overriding function Get_New_Village_Name (
		Form : Form_Type;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Ada.Strings.Fixed.Trim (Web.Element (Inputs, "name"), Ada.Strings.Both);
	end Get_New_Village_Name;
	
	overriding function Get_Text (
		Form : Form_Type;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Ada.Strings.Fixed.Trim (Web.Element (Inputs, "text"), Ada.Strings.Both);
	end Get_Text;
	
end Vampire.Forms.Full;

-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Fixed;
package body Vampire.Forms.Mobile is
	use type Villages.Village_State;
	
	function Create (Speeches_Per_Page : Positive) return Form_Type is
	begin
		return (
			Encoding => iconv.Open (Encoded => "SJIS", Decoded => "UTF-8"),
			Speeches_Per_Page => Speeches_Per_Page);
	end Create;
	
	overriding function HTML_Version (Form : Form_Type) return Web.HTML_Version is
	begin
		return Web.HTML;
	end HTML_Version;
	
	overriding function Template_Set (Form : Form_Type) return Template_Set_Type is
	begin
		return For_Mobile;
	end Template_Set;
	
	overriding function Parameters_To_Index_Page (
		Form : Form_Type;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is
	begin
		return Parameters : Web.Query_Strings do
			Web.Include (Parameters, "b", "k");
			if User_Id'Length > 0 then
				Web.Include (Parameters, "i", User_Id);
				Web.Include (Parameters, "p", User_Password);
			end if;
		end return;
	end Parameters_To_Index_Page;
	
	overriding function Parameters_To_User_Page (
		Form : Form_Type;
		User_Id : String;
		User_Password : String)
		return Web.Query_Strings is
	begin
		return Parameters : Web.Query_Strings do
			Web.Include (Parameters, "b", "k");
			if User_Id'Length > 0 then
				Web.Include (Parameters, "i", User_Id);
				Web.Include (Parameters, "p", User_Password);
			end if;
			Web.Include (Parameters, "u", User_Id);
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
			Web.Include (Parameters, "b", "k");
			if User_Id'Length > 0 then
				Web.Include (Parameters, "i", User_Id);
				Web.Include (Parameters, "p", User_Password);
			end if;
			Web.Include (Parameters, "v", Village_Id);
			if Day >= 0 then
				Web.Include (Parameters, "d", Image (Day));
			end if;
			if First >= 0 and then Last >= 0 then
				Web.Include (Parameters, "r", Image (First) & '-' & Image (Last));
			elsif Latest >= 0 then
				Web.Include (Parameters, "r", Image (Latest));
			end if;
		end return;
	end Parameters_To_Village_Page;
	
	overriding procedure Write_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Item : in String;
		Pre : in Boolean := False) is
	begin
		Web.Write_In_HTML (Stream, Web.HTML, iconv.Encode (Form.Encoding, Item), Pre);
	end Write_In_HTML;
	
	overriding procedure Write_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Item : in String) is
	begin
		Web.Write_In_Attribute (Stream, Web.HTML, iconv.Encode (Form.Encoding, Item));
	end Write_In_Attribute;
	
	overriding function Paging (Form : Form_Type) return Boolean is
	begin
		return True;
	end Paging;
	
	overriding function Speeches_Per_Page (Form : Form_Type) return Natural is
	begin
		return Form.Speeches_Per_Page;
	end Speeches_Per_Page;
	
	overriding function Get_User_Id (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is
	begin
		return Web.Element (Query_Strings, "i");
	end Get_User_Id;
	
	overriding function Get_User_Password (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is
	begin
		return Web.Element (Query_Strings, "p");
	end Get_User_Password;
	
	overriding procedure Set_User (
		Form : in out Form_Type;
		Cookie : in out Web.Cookie;
		New_User_Id: in String;
		New_User_Password : in String) is
	begin
		null;
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
			return Web.Element (Query_Strings, "u") = User_Id;
		end if;
	end Is_User_Page;
	
	overriding function Get_Village_Id (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings)
		return Villages.Village_Id
	is
		S : constant String := Web.Element (Query_Strings, "v");
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
		S : String renames Web.Element (Query_Strings, "d");
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
		Query_Strings : Web.Query_Strings)
		return Villages.Message_Range_Type
	is
		function First_N (N : Natural) return Villages.Message_Range_Type is
			Message_Range : Villages.Message_Range_Type := Village.Message_Range (
				Day,
				Recent_Only => False);
		begin
			return (
				First => Message_Range.First,
				Last => Integer'Max (Message_Range.First,
					Integer'Min (Message_Range.Last, Message_Range.First + (N - 1))));
		end First_N;
		function Last_N (N : Natural) return Villages.Message_Range_Type is
			Message_Range : Villages.Message_Range_Type := Village.Message_Range (
				Day,
				Recent_Only => False);
		begin
			return (
				First => Integer'Max (Message_Range.First, Message_Range.Last - (N - 1)),
				Last => Integer'Max (Message_Range.First, Message_Range.Last));
		end Last_N;
		Range_Arg : constant String := Web.Element (Query_Strings, "r");
		P : constant Natural := Ada.Strings.Fixed.Index (Range_Arg, "-");
	begin
		if P < Range_Arg'First then
			return Last_N (Natural'Value (Range_Arg));
		else
			return (
				First => Natural'Value (Range_Arg (Range_Arg'First .. P - 1)),
				Last => Natural'Value (Range_Arg (P + 1 .. Range_Arg'Last)));
		end if;
	exception
		when Constraint_Error =>
			declare
				State : Villages.Village_State;
				Today : Natural;
			begin
				Village.Get_State (State, Today);
				if State /= Villages.Closed and then Day = Today then
					return Last_N (Form.Speeches_Per_Page);
				else
					return First_N (Form.Speeches_Per_Page);
				end if;
			end;
	end Get_Range;
	
	overriding function Get_New_Village_Name (
		Form : Form_Type;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Ada.Strings.Fixed.Trim (
			iconv.Decode (Form.Encoding, Web.Element (Inputs, "name")),
			Ada.Strings.Both);
	end Get_New_Village_Name;
	
	overriding function Get_Text (
		Form : Form_Type;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Ada.Strings.Fixed.Trim (
			iconv.Decode (Form.Encoding, Web.Element (Inputs, "text")),
			Ada.Strings.Both);
	end Get_Text;
	
end Vampire.Forms.Mobile;

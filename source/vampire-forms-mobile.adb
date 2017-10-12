-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Functions;
with Ada.Environment_Encoding.Names;
with Ada.Environment_Encoding.Encoding_Streams;
package body Vampire.Forms.Mobile is
	use type Villages.Village_State;
	
	Encoding : constant Ada.Environment_Encoding.Encoding_Id :=
		Ada.Environment_Encoding.Names.Windows_31J;
	
	function Create (Speeches_Per_Page : Tabula.Villages.Speech_Positive_Count)
		return Form_Type is
	begin
		return (
			Encoder =>
				new Ada.Environment_Encoding.Strings.Encoder'(
					Ada.Environment_Encoding.Strings.To (Encoding)),
			Decoder =>
				new Ada.Environment_Encoding.Strings.Decoder'(
					Ada.Environment_Encoding.Strings.From (Encoding)),
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
		First : Tabula.Villages.Speech_Index'Base := -1;
		Last : Tabula.Villages.Speech_Index'Base := -1;
		Latest : Tabula.Villages.Speech_Positive_Count'Base := -1;
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
		Pre : in Boolean := False)
	is
		Out_Wrapper : aliased Ada.Environment_Encoding.Encoding_Streams.Out_Type :=
			Ada.Environment_Encoding.Encoding_Streams.Open (
				Ada.Environment_Encoding.Converter (Form.Encoder.all),
				Stream);
	begin
		Web.Write_In_HTML (
			Ada.Environment_Encoding.Encoding_Streams.Stream (Out_Wrapper),
			Web.HTML,
			Item,
			Pre);
		Ada.Environment_Encoding.Encoding_Streams.Finish (Out_Wrapper);
	end Write_In_HTML;
	
	overriding procedure Write_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Item : in String)
	is
		Out_Wrapper : aliased Ada.Environment_Encoding.Encoding_Streams.Out_Type :=
			Ada.Environment_Encoding.Encoding_Streams.Open (
				Ada.Environment_Encoding.Converter (Form.Encoder.all),
				Stream);
	begin
		Web.Write_In_Attribute (
			Ada.Environment_Encoding.Encoding_Streams.Stream (Out_Wrapper),
			Web.HTML,
			Item);
		Ada.Environment_Encoding.Encoding_Streams.Finish (Out_Wrapper);
	end Write_In_Attribute;
	
	overriding function Paging (Form : Form_Type) return Boolean is
	begin
		return True;
	end Paging;
	
	overriding function Speeches_Per_Page (Form : Form_Type)
		return Tabula.Villages.Speech_Positive_Count'Base is
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
		New_User_Id : in String;
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
		Now : Ada.Calendar.Time;
		Query_Strings : Web.Query_Strings)
		return Villages.Speech_Range_Type
	is
		function First_N (N : Tabula.Villages.Speech_Positive_Count'Base)
			return Villages.Speech_Range_Type
		is
			Speech_Range : Villages.Speech_Range_Type :=
				Village.Speech_Range (Day);
		begin
			return (
				First => Speech_Range.First,
				Last => Tabula.Villages.Speech_Index'Base'Max (
					Speech_Range.First,
					Tabula.Villages.Speech_Index'Base'Min (
						Speech_Range.Last,
						Speech_Range.First + (N - 1))));
		end First_N;
		function Last_N (N : Tabula.Villages.Speech_Positive_Count'Base)
			return Villages.Speech_Range_Type
		is
			Speech_Range : Villages.Speech_Range_Type :=
				Village.Speech_Range (Day);
		begin
			return (
				First => Tabula.Villages.Speech_Index'Base'Max (
					Speech_Range.First,
					Speech_Range.Last - (N - 1)),
				Last => Tabula.Villages.Speech_Index'Base'Max (
					Speech_Range.First,
					Speech_Range.Last));
		end Last_N;
		Range_Arg : constant String := Web.Element (Query_Strings, "r");
		P : constant Natural :=
			Ada.Strings.Functions.Index_Element_Forward (Range_Arg, '-');
	begin
		if P < Range_Arg'First then
			return Last_N (Tabula.Villages.Speech_Index'Value (Range_Arg));
		else
			return (
				First => Tabula.Villages.Speech_Index'Value (
					Range_Arg (Range_Arg'First .. P - 1)),
				Last => Tabula.Villages.Speech_Index'Value (
					Range_Arg (P + 1 .. Range_Arg'Last)));
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
		return Ada.Strings.Functions.Trim (
			Ada.Environment_Encoding.Strings.Decode (
				Form.Decoder.all,
				Web.Element (Inputs, "name")),
			Ada.Strings.Both);
	end Get_New_Village_Name;
	
	overriding function Get_Text (
		Form : Form_Type;
		Inputs : Web.Query_Strings)
		return String is
	begin
		return Ada.Strings.Functions.Trim (
			Ada.Environment_Encoding.Strings.Decode (
				Form.Decoder.all,
				Web.Element (Inputs, "text")),
			Ada.Strings.Both);
	end Get_Text;
	
end Vampire.Forms.Mobile;

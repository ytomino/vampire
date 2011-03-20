-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Fixed;
package body Vampire.Forms.Mobile is
	use type Tabula.Villages.Village_State;
	
	function Create return Form_Type is
	begin
		return (Encoding => iconv.Open (Encoded => "SJIS", Decoded => "UTF-8"));
	end Create;
	
	overriding function Get_User_Id(
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String is
	begin
		return Web.Element (Query_Strings, "i");
	end Get_User_Id;
	
	overriding function Get_User_Password(
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
		return Tabula.Villages.Village_Id
	is
		S : constant String := Web.Element (Query_Strings, "v");
	begin
		if S'Length = Tabula.Villages.Village_Id'Length then
			return S;
		else
			return Tabula.Villages.Invalid_Village_Id;
		end if;
	end Get_Village_Id;
	
	overriding function Get_Day (
		Form : Form_Type;
		Village : Villages.Village_Type; 
		Query_Strings : Web.Query_Strings)
		return Natural
	is
		S : String renames Web.Element(Query_Strings, "d");
	begin
		return Natural'Value (S);
	exception
		when Constraint_Error => 
			if Village.State /= Tabula.Villages.Closed then
				return Village.Today;
			else
				return 0;
			end if;
	end Get_Day;
	
	overriding function Get_Range (
		Form : Form_Type;
		Village : Villages.Village_Type; 
		Day : Natural;
		Query_Strings : Web.Query_Strings)
		return Message_Range
	is
		Range_Arg : constant String := Web.Element(Query_Strings, "r");
		P : constant Natural := Ada.Strings.Fixed.Index(Range_Arg, "-");
	begin
		if P < Range_Arg'First then
			declare
				Total : constant Natural := Vampire.Villages.Count_Total_Speech (Village, Day);
			begin
				return (Total - Natural'Value (Range_Arg), Total);
			end;
		else
			return (
				First => Natural'Value (Range_Arg (Range_Arg'First .. P - 1)),
				Last => Natural'Value (Range_Arg (P + 1 .. Range_Arg'Last)));
		end if;
	exception
		when Constraint_Error => 
			if Village.State /= Tabula.Villages.Closed and then Day = Village.Today then
				declare
					Total : constant Natural := Vampire.Villages.Count_Total_Speech (Village, Day);
				begin
					return (Total - Speeches_By_Page, Total);
				end;
			else
				return (0, Speeches_By_Page - 1);
			end if;
	end Get_Range;
	
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

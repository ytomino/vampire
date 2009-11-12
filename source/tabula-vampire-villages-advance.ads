-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Numerics.MT19937;
procedure Tabula.Villages.Advance(
	Village : in out Village_Type;
	Now : in Ada.Calendar.Time;
	Generator : not null access Ada.Numerics.MT19937.Generator;
	Changed : out Boolean;
	List_Changed : out Boolean);

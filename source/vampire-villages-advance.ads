-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Numerics.MT19937;
procedure Vampire.Villages.Advance (
	Village : in out Village_Type;
	Now : in Ada.Calendar.Time;
	Generator : aliased in out Ada.Numerics.MT19937.Generator;
	Changed : out Boolean;
	List_Changed : out Boolean);

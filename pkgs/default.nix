self: super:
{
  adi1090x-plymouth = super.callPackage ./adi1090x-plymouth { };
  lswt = super.callPackage ./lswt { };
  river-tag-overlay = super.callPackage ./river-tag-overlay { };
  swayhide = super.callPackage ./swayhide { };
  swaync = super.callPackage ./swaync { };
  swhkd = super.callPackage ./swhkd { };
}

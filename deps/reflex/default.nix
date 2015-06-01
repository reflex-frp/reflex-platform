{ ref-tf, overrideCabal, reflex }:

overrideCabal reflex (drv: {
  version = "0.2";
  sha256 = "0mq5249dhx26z02jzjr5bxxsgyhg7npd3q7v3l4ipxxxgixg1vdv";
  buildDepends = [ ref-tf ] ++ drv.buildDepends;
})

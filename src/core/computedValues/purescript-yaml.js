import {default as yaml} from 'js-yaml';

export function loadImpl (s)
{
  return yaml.load(s);
}

export function dumpImpl (a)
{
  return yaml.dump(a);
}
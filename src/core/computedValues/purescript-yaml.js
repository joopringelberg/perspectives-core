import {load, dump} from 'js-yaml';

export function loadImpl (s)
{
  return load(s);
}

export function dumpImpl (a)
{
  return dump(a);
}
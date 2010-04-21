package com.locusdev.hlatool;

import scala.Iterator;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Created by IntelliJ IDEA.
 * User: alexfurman
 * Date: Apr 21, 2010
 * Time: 10:45:59 AM
 * To change this template use File | Settings | File Templates.
 */
public class ScalaToJava {
    public static Map<Integer, Set<Character>> convertMutations(scala.collection.immutable.Map<Integer, scala.collection.immutable.Set<Character>> mutations) {
        Map<Integer, Set<Character>> converted = new HashMap<Integer, Set<Character>>();

        Iterator<Integer> keys = mutations.keys();
        while (keys.hasNext()) {
            Integer key = (Integer) keys.next();
            scala.collection.immutable.Set value = (scala.collection.immutable.Set) mutations.get(key).get();
            Iterator<Object> values = value.elements();


            Set<Character> set = new HashSet<Character>();
            while (values.hasNext()) {
                Character c = (Character) values.next();
                set.add(c);
            }
            converted.put(key, set);
        }

        return converted;
    }

    public static Map<String, String> convert(scala.collection.Map<String, String> data) {
        Iterator<String> keys = data.keys();

        Map<String, String> converted = new HashMap<String, String>();

        while (keys.hasNext()) {
            String key = (String) keys.next();
            String value = (String) data.get(key).get();
            converted.put(key, value);
        }
        return converted;

    }
}

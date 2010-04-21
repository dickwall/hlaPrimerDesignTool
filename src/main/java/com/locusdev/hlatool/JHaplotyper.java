package com.locusdev.hlatool;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: alexfurman
 * Date: Apr 21, 2010
 * Time: 2:21:03 PM
 * To change this template use File | Settings | File Templates.
 */
public class JHaplotyper {
    public static int calcs = 0;
    private Map<String, String> data;
    private Map<Integer, Set<Character>> mutations;
    private List<Integer> sortedIndexes;

    public JHaplotyper(Map<String, String> data, Map<Integer, Set<Character>> mutations) {
        this.data = data;
        this.mutations = mutations;
        sortedIndexes = new ArrayList(mutations.keySet());
        Collections.sort(sortedIndexes);
    }

    //private void findAnswers(String sequence, int index, Set<List<JMutation>>)
    private List<JMutation> toListOfMutations(String sequence) {
        List<JMutation> mutations = new ArrayList<JMutation>(sortedIndexes.size());
        for (int x = 0; x < sortedIndexes.size(); x++) {
            mutations.add(new JMutation(x, sequence.charAt(x)));
        }
        return mutations;
    }

    private Set<List<JMutation>> findAllUniqueSignatures(String allele) {
        String currentSequence = data.get(allele);
        List<JMutation> mutations = toListOfMutations(currentSequence);

        Set<List<JMutation>> answers = new HashSet<List<JMutation>>();
        //go through the sequence mutation by mutation and look for uniquely identifying strings

        for (JMutation mutation : mutations) {
            List<JMutation> rest = new ArrayList<JMutation>();
            rest.addAll(mutations);
            rest.remove(mutation);
            List<JMutation> current = new ArrayList<JMutation>(1);
            current.add(mutation);

            List<String> otherSequences = new ArrayList(data.values());
            otherSequences.remove(currentSequence);

            System.out.println("iteration");
            findUniqueSequence(answers, current, rest, otherSequences);
        }
        return (answers);
    }

    private void findUniqueSequence(Set<List<JMutation>> answers, List<JMutation> handled, List<JMutation> unhandled, List<String> sequences) {
        // eliminate all the sequences that don't match the mutations in "handled"

        //if we're over our answer length, don't do anything
        if (handled.size() <= 5) {


            if (calcs++ % 10000 == 0) {
                System.out.println("calcs: " + calcs);
            }
//            System.out.println("handled  : " + handled.size());
//            System.out.println("unhandled: " + unhandled.size());
//            System.out.println("answers  : " + answers.size());


            List<String> filteredSequences = eliminateSequences(handled, sequences);
            if (filteredSequences.size() == 0) {
                answers.add(handled);
            } else {
                //recurse down, picking one of the remaining mutations
                for (JMutation mutation : unhandled) {
                    List<JMutation> newHandled = new ArrayList<JMutation>();
                    newHandled.addAll(handled);
                    newHandled.add(mutation);

                    List<JMutation> newUnhandled = new ArrayList<JMutation>();
                    newUnhandled.addAll(unhandled);
                    newUnhandled.remove(mutation);

                    findUniqueSequence(answers, newHandled, newUnhandled, filteredSequences);
                }
            }
        }


    }

    private List<String> eliminateSequences(List<JMutation> mutations, List<String> sequences) {
        List<String> filtered = new ArrayList<String>();
        for (String sequence : sequences) {
            if (matches(sequence, mutations)) {
                filtered.add(sequence);
            }
        }
        return filtered;
    }

    private boolean matches(String sequence, List<JMutation> mutations) {
        for (JMutation mutation : mutations) {
            if (!matches(sequence, mutation)) {
                return false;
            }
        }

        return true;
    }

    private boolean matches(String sequence, JMutation mutation) {
        return sequence.charAt(mutation.getIndex()) == mutation.getNucleotide();
    }


    public static void main(String[] args) throws FileNotFoundException {
        scala.collection.Map<String, String> data = DbMhcParser.extractSequence(
                new FileReader("/Users/alexfurman/projects/hlaPrimerDesignTool/src/main/resources/dbMHC_allelev2.28.xml"),
                "HLA-B", "Exon3", 0);
        Haplotyper haplotyper = new Haplotyper();
        scala.collection.immutable.Map<Integer, scala.collection.immutable.Set<Character>> mutations =
                haplotyper.mutationMap(haplotyper.findAllMutations(data.values().toList()));


        Map<String, String> map = ScalaToJava.convert(data);

        int line = 0;
        for (String s : map.values()) {
            int count = 0;
            for (int x = 0; x < s.length(); x++) {
                if (s.charAt(x) == '*') {
                    count++;
                }
            }
            line++;
            if (count > 0) {
                System.out.println("line: " + (line) + ", count: " + count);
                System.out.println(s);
            }
        }

        new JHaplotyper(map, ScalaToJava.convertMutations(mutations)).findAllUniqueSignatures("B*0766");


    }

}

class JMutation {
    private int index;
    private char nucleotide;

    JMutation(int index, char nucleotide) {
        this.index = index;
        this.nucleotide = nucleotide;
    }

    public int getIndex() {
        return index;
    }

    public char getNucleotide() {
        return nucleotide;
    }

    @Override
    public String toString() {
        return "JMutation{" +
                "index=" + index +
                ", nucleotide=" + nucleotide +
                '}';
    }
}
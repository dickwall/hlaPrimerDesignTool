package com.locusdev.hlatool;

import com.google.common.base.Predicate;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: alexfurman
 * Date: Apr 21, 2010
 * Time: 2:21:03 PM
 */
public class JHaplotyper {
    public static int calcs = 0;
    private Map<String, String> data;
    private Map<Integer, Set<Character>> mutations;
    private List<Integer> sortedIndexes;

    public JHaplotyper(Map<String, String> data, Map<Integer, Set<Character>> mutations) {
        this.data = data;
        lookForDuplicates(data);
        this.mutations = mutations;
        sortedIndexes = new ArrayList(mutations.keySet());
        Collections.sort(sortedIndexes);
    }

    private void lookForDuplicates(Map<String, String> data) {
        Multimap<String, String> multimap = HashMultimap.create();
        for (Map.Entry<String, String> entry : data.entrySet()) {
            multimap.put(entry.getValue(), entry.getKey());
        }
        //filter out anything that has length 1
        Multimap<String, String> filteres = HashMultimap.create();

        for (String key : multimap.keySet()) {
            Collection<String> values = multimap.get(key);
            if (values.size() > 1) {
                System.out.println(key);
                System.out.println(values);
            }
        }

    }

    private void findAnswer(String allelePrefix) {
        //first, pull out all of the alleles that match the prefix we care about
        final List<JAllele> alleles = getMatchingAlleles(allelePrefix);
        if (alleles.size() == 0) {
            throw new IllegalStateException("Not alleles start with prefix: " + allelePrefix);
        }

        final List<JAllele> otherAlleles = getNonMatchingAlleles(allelePrefix);

        //now, get the mutations that these alleles have in common
        List<JMutation> commonMutations = getCommonMutations(alleles);

        //now, ensure that the common mutations will uniquely identify alleles that are in this group
        List<JAllele> alsoMatches = new ArrayList<JAllele>();
        for (JAllele allele : otherAlleles) {
            if (allele.matchesMutations(commonMutations)) {
                alsoMatches.add(allele);
            }
        }
        if (alsoMatches.size() > 0) {
            throw new IllegalStateException("Common mutations for group \"" + allelePrefix + "\" also matches " +
                    alsoMatches);
        }
        final Map<JMutation, Integer> mutationFrequencies = getMutationFrequencies(otherAlleles, commonMutations);

        sortByMutationFrequencies(otherAlleles, commonMutations);

        //guess at answer using a greedy algorithm
        List<JMutation> guess = greedyGuessAnswer(commonMutations, otherAlleles);

        System.out.println("Guess: (" + guess.size() + ") - " + guess);


    }

    private List<JMutation> sortByMutationFrequencies(List<JAllele> otherAlleles, List<JMutation> commonMutations) {
        //now, we can figure out how common each of the mutations is in the general data
        final Map<JMutation, Integer> mutationFrequencies = getMutationFrequencies(otherAlleles, commonMutations);
        //sort the mutations according to how rare they are in the alleles
        Collections.sort(commonMutations, new Comparator<JMutation>() {

            @Override
            public int compare(JMutation o1, JMutation o2) {
                return mutationFrequencies.get(o1).compareTo(mutationFrequencies.get(o2));
            }
        });

        //print 10 least frequent mutations
        printFirst10(commonMutations, mutationFrequencies);

        return commonMutations;
    }

    private List<JMutation> greedyGuessAnswer(List<JMutation> mutations, List<JAllele> alleles) {
        List<JMutation> guess = new ArrayList<JMutation>();

        //copy the other alleles into a separate list we're going to be messing with
        List<JAllele> allelesLeft = new ArrayList<JAllele>(alleles.size());
        allelesLeft.addAll(alleles);

        //copy the mutations into a separate list
        List<JMutation> commonMutations = new ArrayList<JMutation>(mutations.size());
        commonMutations.addAll(mutations);

        int index = 0;
        while (allelesLeft.size() != 0 && index < mutations.size()) {
            sortByMutationFrequencies(allelesLeft, commonMutations);
            guess.add(commonMutations.remove(0));
            allelesLeft = eliminateAlleles(guess, allelesLeft);
        }
        return guess;
    }

    private void printFirst10(List<JMutation> mutations, Map<JMutation, Integer> mutationFrequencies) {
        System.out.println("First 10 mutations and their frequencies");
        for (int x = 0; x < 10 && x < mutations.size(); x++) {
            JMutation mutation = mutations.get(x);
            System.out.println("\t" + mutation + " - " + mutationFrequencies.get(mutation));
        }
    }

    private Map<JMutation, Integer> getMutationFrequencies(List<JAllele> alleles, List<JMutation> mutations) {
        Map<JMutation, Integer> mutationFrequencies = new HashMap<JMutation, Integer>();
        for (JMutation commonMutation : mutations) {
            int mutationCounter = 0;
            for (JAllele otherAllele : alleles) {
                if (otherAllele.matchesMutation(commonMutation)) {
                    mutationCounter++;
                }
            }
            mutationFrequencies.put(commonMutation, mutationCounter);
        }
        return mutationFrequencies;
    }

    private List<JMutation> getCommonMutations(final List<JAllele> alleles) {
        ArrayList<Integer> indexes = Lists.newArrayList(Iterables.filter(sortedIndexes, new Predicate<Integer>() {
            public boolean apply(Integer input) {
                if (input == null) {
                    return false;
                }
                JAllele first = alleles.get(0);

                if (alleles.size() == 1) {
                    return first.baseSequenced(input);
                }

                for (int x = 1; x < alleles.size(); x++) {
                    if (first.getNucleotide(input) != alleles.get(x).getNucleotide(input)) {
                        return false;
                    }
                }

                return true;
            }
        }));
        //now, turn those into mutations
        return extractMutations(alleles.get(0), indexes);
    }

    /**
     * returns all the alleles whose name starts with the specified prefix
     *
     * @param allelePrefix prefix for the allele name (e.g. B*1234)
     * @return list of all alleles that match the prefix
     */
    private List<JAllele> getMatchingAlleles(String allelePrefix) {
        List<JAllele> alleles = new ArrayList<JAllele>();
        for (Map.Entry<String, String> entry : data.entrySet()) {
            if (entry.getKey().startsWith(allelePrefix)) {
                alleles.add(new JAllele(entry.getKey(), entry.getValue()));
            }
        }
        return alleles;
    }

    private List<JAllele> getNonMatchingAlleles(String allelePrefix) {
        List<JAllele> alleles = new ArrayList<JAllele>();
        for (Map.Entry<String, String> entry : data.entrySet()) {
            if (!entry.getKey().startsWith(allelePrefix)) {
                alleles.add(new JAllele(entry.getKey(), entry.getValue()));
            }
        }
        return alleles;
    }

    private List<JMutation> extractMutations(JAllele allele, List<Integer> mutationIndexes) {
        List<JMutation> mutations = new ArrayList<JMutation>(mutationIndexes.size());
        for (Integer mutationIndexe : mutationIndexes) {
            mutations.add(new JMutation(mutationIndexe, allele.getNucleotide(mutationIndexe)));
        }
        return mutations;
    }

    private List<JAllele> eliminateAlleles(List<JMutation> mutations, List<JAllele> alleles) {
        List<JAllele> filtered = new ArrayList<JAllele>();
        for (JAllele allele : alleles) {
            if (JMutation.matches(allele, mutations)) {
                filtered.add(allele);
            }
        }
        return filtered;
    }


    public static void main(String[] args) throws FileNotFoundException {
        String[] blocks = {"Exon2", "Exon3"};
        scala.collection.Map<String, String> data = DbMhcParser.extractSequence(
                new FileReader("/Users/alexfurman/projects/hlaPrimerDesignTool/src/main/resources/dbMHC_allelev2.28.xml"),
                "HLA-B", blocks, 0);
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

        new JHaplotyper(map, ScalaToJava.convertMutations(mutations)).findAnswer("B*1507");


    }


//     private Set<List<JMutation>> findAllUniqueSignatures(String allele) {
//        String currentSequence = data.get(allele);
//        List<JMutation> mutations = extractMutations(currentSequence);
//
//        Set<List<JMutation>> answers = new HashSet<List<JMutation>>();
//        //go through the sequence mutation by mutation and look for uniquely identifying strings
//
//        for (JMutation mutation : mutations) {
//            List<JMutation> rest = new ArrayList<JMutation>();
//            rest.addAll(mutations);
//            rest.remove(mutation);
//            List<JMutation> current = new ArrayList<JMutation>(1);
//            current.add(mutation);
//
//            List<String> otherSequences = new ArrayList(data.values());
//            otherSequences.remove(currentSequence);
//
//            System.out.println("iteration");
//            findUniqueSequence(answers, current, rest, otherSequences);
//        }
//        return (answers);
//    }
//
//    private void findUniqueSequence(Set<List<JMutation>> answers, List<JMutation> handled, List<JMutation> unhandled, List<String> sequences) {
//        // eliminate all the sequences that don't match the mutations in "handled"
//
//        //if we're over our answer length, don't do anything
//        if (handled.size() <= 5) {
//
//
//            if (calcs++ % 10000 == 0) {
//                System.out.println("calcs: " + calcs);
//            }
////            System.out.println("handled  : " + handled.size());
////            System.out.println("unhandled: " + unhandled.size());
////            System.out.println("answers  : " + answers.size());
//
//
//            List<String> filteredSequences = eliminateAlleles(handled, sequences);
//            if (filteredSequences.size() == 0) {
//                answers.add(handled);
//            } else {
//                //recurse down, picking one of the remaining mutations
//                for (JMutation mutation : unhandled) {
//                    List<JMutation> newHandled = new ArrayList<JMutation>();
//                    newHandled.addAll(handled);
//                    newHandled.add(mutation);
//
//                    List<JMutation> newUnhandled = new ArrayList<JMutation>();
//                    newUnhandled.addAll(unhandled);
//                    newUnhandled.remove(mutation);
//
//                    findUniqueSequence(answers, newHandled, newUnhandled, filteredSequences);
//                }
//            }
//        }
//
//
//    }

}

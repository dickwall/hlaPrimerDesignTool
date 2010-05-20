package com.locusdev.hlatool;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: alexfurman
 * Date: Apr 21, 2010
 * Time: 2:28:47 PM
 * To change this template use File | Settings | File Templates.
 */
public class JMutation implements Comparable<JMutation> {
    private Integer index;
    private Character nucleotide;

    JMutation(int index, char nucleotide) {
        this.index = index;
        this.nucleotide = nucleotide;
    }

    public Integer getIndex() {
        return index;
    }

    public Character getNucleotide() {
        return nucleotide;
    }

    @Override
    public String toString() {
        return "[" +
                index +
                ":" + nucleotide +
                ']';
    }

    public boolean matches(String sequence) {
        return sequence.charAt(getIndex()) == getNucleotide();
    }

    public static boolean matches(String sequence, List<JMutation> mutations) {
        for (JMutation mutation : mutations) {
            if (!mutation.matches(sequence)) {
                return false;
            }
        }

        return true;
    }

    @Override
    public int compareTo(JMutation o) {
        return getIndex().compareTo(o.getIndex());
    }

    public static boolean matches(JAllele allele, List<JMutation> mutations) {
        return matches(allele.getSequence(), mutations);
    }
}
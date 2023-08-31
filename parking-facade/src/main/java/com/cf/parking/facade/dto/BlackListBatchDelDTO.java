package com.cf.parking.facade.dto;

import lombok.Data;

import java.util.List;

/**
 * @author: lpy
 * @Date: 2023/03/27
 */
@Data
public class BlackListBatchDelDTO {
    /**
     * 批量删除的id
     */
    private List<Long> blackListIds;
}

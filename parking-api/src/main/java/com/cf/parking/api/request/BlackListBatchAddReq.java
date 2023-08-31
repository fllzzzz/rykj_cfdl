package com.cf.parking.api.request;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author: lpy
 * @Date: 2023/03/31
 */
@Data
@Accessors(chain = true)
public class BlackListBatchAddReq {
    /**
     * 黑名单批量添加
     */
    private List<BlackListBatchAdditionReq> list;
}
